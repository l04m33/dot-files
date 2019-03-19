#ifndef D_MACRO_H_INCLUDED
#define D_MACRO_H_INCLUDED

#include "action.h"
#include "timer.h"
#include "wait.h"

#ifdef DEBUG_D_MACRO
#   include "debug.h"
#else
#   include "nodebug.h"
#endif


#ifdef STACK_USAGE
#define STACK_SCAN_INTERVAL 2000
#define STACK_COLOR         0x99
static uint8_t *stack_begin, *stack_end;
#endif


#define MAX_D_MACRO_EVENTS 256
#define D_MACRO_EV_SUFFIX_LEN 3


typedef enum {
    D_MACRO_STATE_IDLE,
    D_MACRO_STATE_RECORDING,
    D_MACRO_STATE_READY,
    D_MACRO_STATE_PLAYING,
} d_macro_state_t;

typedef struct {
    d_macro_state_t state;
    int16_t         play_interval;
    keypos_t        rec_key;
    uint16_t        ev_count;
    keyevent_t      ev[MAX_D_MACRO_EVENTS];
} d_macro_t;

static d_macro_t d_macro = {
    .state   = D_MACRO_STATE_IDLE,
    .play_interval = -1,
    .rec_key = { .row = 0, .col = 0 },
    .ev_count = 0,
};


static void d_macro_action_record(keyrecord_t *record)
{
    if (d_macro.state == D_MACRO_STATE_IDLE) {
        if (!record->event.pressed && KEY_TAPPED(record, 1)) {
            dprintln("Start dynamic macro recording");
            d_macro.state = D_MACRO_STATE_RECORDING;
            d_macro.ev_count = 0;
            d_macro.rec_key = record->event.key;
        }
    } else if (d_macro.state == D_MACRO_STATE_RECORDING) {
        if (!record->event.pressed && KEY_TAPPED(record, 1)) {
            dprintln("Stop dynamic macro recording");
            d_macro.state = D_MACRO_STATE_IDLE;
            /*
             * XXX: Dirty hack. Strip the extra macro events to prevent unwanted
             *      layer switch. Only works with this layout set-up, when the
             *      macro recording is stopped by typing
             *      L1 down -> MREC down -> MREC up -> L1 up.
             */
            d_macro.ev_count -=
                d_macro.ev_count > D_MACRO_EV_SUFFIX_LEN ?
                D_MACRO_EV_SUFFIX_LEN : d_macro.ev_count;
        }
    }
}

static void d_macro_action_play(keyrecord_t *record, int16_t interval)
{
    if (d_macro.state != D_MACRO_STATE_IDLE) {
        return;
    }

    if (!record->event.pressed && KEY_TAPPED(record, 1)) {
        dprintln("Schedule dynamic macro replay");
        d_macro.state = D_MACRO_STATE_READY;
        d_macro.play_interval = interval;
    }
}

static void d_macro_hook_matrix_change(keyevent_t event)
{
    if (d_macro.state != D_MACRO_STATE_RECORDING || d_macro.ev_count >= MAX_D_MACRO_EVENTS) {
        return;
    }

    if (d_macro.ev_count <= 0 && !event.pressed) {
        // stale key release event(s), ignore
        return;
    }

    if (d_macro.ev_count > 0) {
        keyevent_t *last_ev = &(d_macro.ev[d_macro.ev_count - 1]);
        if ((last_ev->pressed == event.pressed) &&
            (last_ev->key.row == event.key.row) &&
            (last_ev->key.col == event.key.col) &&
            (last_ev->time == event.time)) {
            // duplicate events, ignore
            return;
        }
    }

    d_macro.ev[d_macro.ev_count] = event;
    dprintf("Recorded key event %u: (%u,%u) %s [%u]\n",
            d_macro.ev_count, event.key.row, event.key.col,
            event.pressed ? "down" : "up", event.time);
    (d_macro.ev_count)++;
}

static void d_macro_stack_scan(void)
{
#ifdef STACK_USAGE
    static uint16_t last_stack_scan = 0;

    uint16_t cur_time = timer_read();
    if (TIMER_DIFF_16(cur_time, last_stack_scan) >= STACK_SCAN_INTERVAL) {
        last_stack_scan = cur_time;

        uint8_t *to_check;
        for (to_check = stack_end; to_check <= stack_begin; to_check++) {
            if (*to_check != STACK_COLOR) {
                break;
            }
        }

        xprintf("Stack scan: to_check = 0x%04X\n", (uint16_t)to_check);
    }
#endif
}

static void dyn_wait_ms(uint16_t ms)
{
    uint16_t start_time = timer_read();
    while (TIMER_DIFF_16(timer_read(), start_time) < ms) {
        wait_ms(1);
    }
}

static void d_macro_hook_keyboard_loop(void)
{
    if (d_macro.state != D_MACRO_STATE_READY) {
        return;
    }

    int16_t play_interval = d_macro.play_interval;
    uint16_t last_ts = 0;
    if (play_interval < 0 && d_macro.ev_count > 0) {
        last_ts = d_macro.ev[0].time;
    }

    uint32_t prev_layer_state = layer_state;
    layer_state = 0;
    d_macro.state = D_MACRO_STATE_PLAYING;

    dprintln("Start playing macro events");
    for (uint8_t i = 0; i < d_macro.ev_count; i++) {
        if (play_interval < 0) {
            dprintf("  Waiting for %u ms\n", TIMER_DIFF_16(d_macro.ev[i].time, last_ts));
            dyn_wait_ms(TIMER_DIFF_16(d_macro.ev[i].time, last_ts));
            last_ts = d_macro.ev[i].time;
        } else {
            dprintf("  Waiting for %u ms\n", play_interval);
            dyn_wait_ms(play_interval);
        }

        dprintf("  Playing macro event %u\n", i);
        action_exec(d_macro.ev[i]);
        dprintf("  Done playing macro event %u\n", i);
    }
    dprintln("Stop playing macro events");

    d_macro.state = D_MACRO_STATE_IDLE;
    layer_state = prev_layer_state;
}

static action_t d_macro_action_for_key(keypos_t key, action_t action)
{
    if (d_macro.state == D_MACRO_STATE_RECORDING &&
        d_macro.ev_count >= MAX_D_MACRO_EVENTS &&
        // the key to stop recording should always work
        !KEYEQ(key, d_macro.rec_key) &&
        // layer actions should always work, in case the stopping key is in a hidden layer
        !(action.kind.id == ACT_LAYER ||
            action.kind.id == ACT_LAYER_TAP ||
            action.kind.id == ACT_LAYER_TAP_EXT)) {
        // macro buffer full, drop all key actions to signify this
        dprintln("Macro buffer full, ignore key action");
        return (action_t)ACTION_NO;
    }
    return action;
}


/*
 * stack usage
 */

extern uint8_t _end;

void d_macro_stack_scan_hook_late_init(void)
{
#ifdef STACK_USAGE
    static uint8_t *to_paint;
    uint8_t dummy;

    stack_begin = &dummy;
    stack_end   = &_end;
    xprintf("Painting stack, from 0x%04X down to 0x%04X\n", (uint16_t)stack_begin, (uint16_t)stack_end);

    for (to_paint = stack_begin; to_paint >= stack_end; to_paint--) {
        *to_paint = STACK_COLOR;
    }

    xprintf("Done painting stack\n");
#endif // STACK_USAGE
}

#endif // D_MACRO_H_INCLUDED

