/*
Copyright 2016 Jun Wako <wakojun@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/*
 * l04m33 custom keymap, based on unimap_hhkb.c
 */

#include "keymap.h"
#include "keyboard.h"
#include "action.h"
#include "timer.h"
#include "wait.h"
#include "unimap_trans.h"
#if defined(__AVR__)
#   include <avr/pgmspace.h>
#endif


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


enum function_id {
    D_MACRO_FUNC_RECORD,
    D_MACRO_FUNC_PLAY,
    FUNC_LSHIFT_LPAREN,
    FUNC_RSHIFT_RPAREN,
};


#define AC_L1      ACTION_LAYER_TAP_TOGGLE(1)
#define AC_L2      ACTION_LAYER_TAP_KEY(2, KC_SPC)
#define AC_CTLENT  ACTION_MODS_TAP_KEY(MOD_RCTL, KC_ENT)
#define AC_MREC    ACTION_FUNCTION_TAP(D_MACRO_FUNC_RECORD)
#define AC_MPLAY   ACTION_FUNCTION_TAP(D_MACRO_FUNC_PLAY)
#define AC_LOCK    ACTION_LAYER_TAP_TOGGLE(3)
#define AC_UNLOCK  ACTION_LAYER_MOMENTARY(4)
#define AC_LSFTPRN ACTION_FUNCTION_TAP(FUNC_LSHIFT_LPAREN)
#define AC_RSFTPRN ACTION_FUNCTION_TAP(FUNC_RSHIFT_RPAREN)

#ifdef KEYMAP_SECTION_ENABLE
const action_t actionmaps[][UNIMAP_ROWS][UNIMAP_COLS] __attribute__ ((section (".keymap.keymaps"))) = {
#else
const action_t actionmaps[][UNIMAP_ROWS][UNIMAP_COLS] PROGMEM = {
#endif
    /* layer 0: default layer
     * ,-----------------------------------------------------------.
     * |Esc|  1|  2|  3|  4|  5|  6|  7|  8|  9|  0|  -|  =|  \|  `|
     * |-----------------------------------------------------------|
     * |Tab  |  Q|  W|  E|  R|  T|  Y|  U|  I|  O|  P|  [|  ]|Backs|
     * |-----------------------------------------------------------|
     * |Contro|  A|  S|  D|  F|  G|  H|  J|  K|  L|  ;|  '| CTLENT |
     * |-----------------------------------------------------------|
     * |Shift   |  Z|  X|  C|  V|  B|  N|  M|  ,|  .|  /|Shift |L1 |
     * `-----------------------------------------------------------'
     *       |Gui| Alt |          L2           | Alt |Gui|
     *       `-------------------------------------------'
     */
    [0] = UNIMAP_HHKB(
    ESC,     1,    2,    3,   4,   5,   6,   7,   8,    9,    0,    MINS,    EQL,    BSLS, GRV,
    TAB,     Q,    W,    E,   R,   T,   Y,   U,   I,    O,    P,    LBRC,    RBRC,   BSPC,
    LCTL,    A,    S,    D,   F,   G,   H,   J,   K,    L,    SCLN, QUOT,    CTLENT,
    LSFTPRN, Z,    X,    C,   V,   B,   N,   M,   COMM, DOT,  SLSH, RSFTPRN, L1,
             LGUI, LALT,           L2,                  RALT, RGUI),

    /* layer 1: hhkb mode (hhkb fn) */
    [1] = UNIMAP_HHKB(
    PWR,  F1,   F2,   F3,   F4,   F5,   F6,   F7,   F8,   F9,   F10,  F11,  F12,  INS,  DEL,
    CAPS, TRNS, TRNS, TRNS, TRNS, TRNS, TRNS, TRNS, PSCR, SLCK, PAUS, UP,   TRNS, BSPC,
    TRNS, VOLD, VOLU, MUTE, TRNS, TRNS, PAST, PSLS, HOME, PGUP, LEFT, RGHT, PENT,
    LOCK, TRNS, TRNS, TRNS, TRNS, TRNS, PPLS, PMNS, END,  PGDN, DOWN, TRNS, L1,
          TRNS, TRNS,             SPC,                    MREC, MPLAY),

    /* layer 2: vi movement keys and mouse keys (space) */
    [2] = UNIMAP_HHKB(
    ESC,  F1,   F2,   F3,   F4,   F5,   F6,   F7,   F8,   F9,   F10,  F11,  F12,    INS,  DEL,
    TAB,  TRNS, WH_L, WH_U, WH_D, WH_R, HOME, PGDN, PGUP, END,  TRNS, TRNS, TRNS,   BSPC,
    LCTL, TRNS, MS_L, MS_U, MS_D, MS_R, LEFT, DOWN, UP,   RGHT, TRNS, TRNS, CTLENT,
    LSFT, TRNS, BTN3, BTN2, BTN1, TRNS, ACL2, ACL1, ACL0, TRNS, TRNS, RSFT, TRNS,
          LGUI, LALT,             L2,                     RALT, RGUI),

    /* layer 3: locked */
    [3] = UNIMAP_HHKB(
    NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO,
    NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO,
    NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO,
    NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, UNLOCK,
        NO, NO,             NO,                     NO, NO),

    /* layer 4: unlocking */
    [4] = UNIMAP_HHKB(
    NO,   NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO,
    NO,   NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO,
    NO,   NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO,
    LOCK, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, UNLOCK,
          NO, NO,             NO,                     NO, NO),
};


typedef enum {
    D_MACRO_STATE_IDLE,
    D_MACRO_STATE_RECORDING,
    D_MACRO_STATE_READY,
    D_MACRO_STATE_PLAYING,
} d_macro_state_t;

typedef struct {
    d_macro_state_t state;
    keypos_t        rec_key;
    uint16_t        ev_count;
    keyevent_t      ev[MAX_D_MACRO_EVENTS];
} d_macro_t;

static d_macro_t d_macro = {
    .state   = D_MACRO_STATE_IDLE,
    .rec_key = { .row = 0, .col = 0 },
    .ev_count = 0,
};

#define KEY_TAPPED(_rec_, _count_) ((_rec_)->tap.count == (_count_) && !(_rec_)->tap.interrupted)

static void action_record(keyrecord_t *record)
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

static void action_play(keyrecord_t *record)
{
    if (d_macro.state != D_MACRO_STATE_IDLE) {
        return;
    }

    if (!record->event.pressed && KEY_TAPPED(record, 1)) {
        dprintln("Schedule dynamic macro replay");
        d_macro.state = D_MACRO_STATE_READY;
    }
}

static void action_shift_paren(keyrecord_t *record, enum hid_keyboard_keypad_usage shift_kc)
{
    if (record->event.pressed) {
        if (record->tap.count <= 0 || record->tap.interrupted) {
            register_mods(MOD_BIT(shift_kc));
        }
    } else {
        if (record->tap.count > 0 && !record->tap.interrupted) {
            add_weak_mods(MOD_BIT(shift_kc));
            send_keyboard_report();
            switch (shift_kc) {
                case KC_LSHIFT:
                    register_code(KC_9);
                    unregister_code(KC_9);
                    break;
                case KC_RSHIFT:
                    register_code(KC_0);
                    unregister_code(KC_0);
                    break;
                default:
                    break;
            }
            del_weak_mods(MOD_BIT(shift_kc));
            send_keyboard_report();
        } else {
            unregister_mods(MOD_BIT(shift_kc));
        }
    }
}

void action_function(keyrecord_t *record, uint8_t id, uint8_t opt)
{
    switch (id) {
        case D_MACRO_FUNC_RECORD:
            action_record(record);
            break;
        case D_MACRO_FUNC_PLAY:
            action_play(record);
            break;
        case FUNC_LSHIFT_LPAREN:
            action_shift_paren(record, KC_LSHIFT);
            break;
        case FUNC_RSHIFT_RPAREN:
            action_shift_paren(record, KC_RSHIFT);
            break;
        default:
            break;
    }
}

void hook_matrix_change(keyevent_t event)
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

static void dyn_wait_ms(uint16_t ms)
{
    uint16_t start_time = timer_read();
    while (TIMER_DIFF_16(timer_read(), start_time) < ms) {
        wait_ms(1);
    }
}

void hook_keyboard_loop(void)
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

    if (d_macro.state != D_MACRO_STATE_READY) {
        return;
    }

    uint16_t last_ts = 0;
    if (d_macro.ev_count > 0) {
        last_ts = d_macro.ev[0].time;
    }

    uint32_t prev_layer_state = layer_state;
    layer_state = 0;
    d_macro.state = D_MACRO_STATE_PLAYING;

    dprintln("Start playing macro events");
    for (uint8_t i = 0; i < d_macro.ev_count; i++) {
        dprintf("  Waiting for %u ms\n", TIMER_DIFF_16(d_macro.ev[i].time, last_ts));
        dyn_wait_ms(TIMER_DIFF_16(d_macro.ev[i].time, last_ts));
        last_ts = d_macro.ev[i].time;

        dprintf("  Playing macro event %u\n", i);
        action_exec(d_macro.ev[i]);
        dprintf("  Done playing macro event %u\n", i);
    }
    dprintln("Stop playing macro events");

    d_macro.state = D_MACRO_STATE_IDLE;
    layer_state = prev_layer_state;
}

#ifdef BOOTMAGIC_ENABLE

#define BOOTMAGIC_CUSTOM_KEY_SWAP_ALT_GUI KC_Z

extern keymap_config_t keymap_config;

void hook_bootmagic(void)
{
    if (bootmagic_scan_key(BOOTMAGIC_CUSTOM_KEY_SWAP_ALT_GUI)) {
        keymap_config.raw = eeconfig_read_keymap();
        keymap_config.swap_lalt_lgui = !keymap_config.swap_lalt_lgui;
        keymap_config.swap_ralt_rgui = keymap_config.swap_lalt_lgui;
        eeconfig_write_keymap(keymap_config.raw);
    }
}

static action_t handle_bootmagic_key_swaps(action_t action)
{
    if (action.kind.id != ACT_LMODS && action.kind.id != ACT_RMODS)
    {
        return action;
    }

    switch (action.key.code) {
        case KC_LALT:
            if (keymap_config.swap_lalt_lgui) {
                action.key.code = KC_LGUI;
            }
            break;
        case KC_LGUI:
            if (keymap_config.swap_lalt_lgui) {
                action.key.code = KC_LALT;
            }
            break;
        case KC_RALT:
            if (keymap_config.swap_ralt_rgui) {
                action.key.code = KC_RGUI;
            }
            break;
        case KC_RGUI:
            if (keymap_config.swap_ralt_rgui) {
                action.key.code = KC_RALT;
            }
            break;
        default:
            break;
    }

    return action;
}

#endif // BOOTMAGIC_ENABLE

extern keypos_t unimap_translate(keypos_t key);

action_t action_for_key(uint8_t layer, keypos_t key)
{
    keypos_t uni = unimap_translate(key);
    if ((uni.row << 4 | uni.col) == UNIMAP_NO) {
        return (action_t)ACTION_NO;
    }

    action_t action =
#if defined(__AVR__)
        (action_t)pgm_read_word(&actionmaps[(layer)][(uni.row & 0x7)][(uni.col)]);
#else
        actionmaps[(layer)][(uni.row & 0x7)][(uni.col)];
#endif

#ifdef BOOTMAGIC_ENABLE
    action = handle_bootmagic_key_swaps(action);
#endif

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

#ifdef STACK_USAGE

extern uint8_t _end;

void hook_late_init(void)
{
    static uint8_t *to_paint;
    uint8_t dummy;

    stack_begin = &dummy;
    stack_end   = &_end;
    xprintf("Painting stack, from 0x%04X down to 0x%04X\n", (uint16_t)stack_begin, (uint16_t)stack_end);

    for (to_paint = stack_begin; to_paint >= stack_end; to_paint--) {
        *to_paint = STACK_COLOR;
    }

    xprintf("Done painting stack\n");
}

#endif
