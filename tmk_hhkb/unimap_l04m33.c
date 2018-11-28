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
#include "eeconfig.h"
#include "bootmagic.h"
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

typedef struct {
    uint8_t key_code;
    uint8_t modifiers;
} penti_chord_map_entry_t;

#ifdef KEYMAP_SECTION_ENABLE
static const penti_chord_map_entry_t penti_alpha_chord_map[] __attribute__ ((section (".keymap.keymaps"))) = {
#else
static const penti_chord_map_entry_t penti_alpha_chord_map[] PROGMEM = {
#endif
    { .key_code = KC_NO },     // 0x00
    { .key_code = KC_SPACE },
    { .key_code = KC_S },
    { .key_code = KC_F },
    { .key_code = KC_E },
    { .key_code = KC_R },
    { .key_code = KC_L },
    { .key_code = KC_Z },
    { .key_code = KC_I },
    { .key_code = KC_A },
    { .key_code = KC_C },
    { .key_code = KC_Q },
    { .key_code = KC_O },
    { .key_code = KC_B },
    { .key_code = KC_U },
    { .key_code = KC_P },
    { .key_code = KC_N },      // 0x10
    { .key_code = KC_D },
    { .key_code = KC_J },
    { .key_code = KC_H },
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_G },
    { .key_code = KC_Y },
    { .key_code = KC_V },
    { .key_code = KC_X },
    { .key_code = KC_M },
    { .key_code = KC_T },
    { .key_code = KC_K },
    { .key_code = KC_W },
};

#ifdef KEYMAP_SECTION_ENABLE
static const penti_chord_map_entry_t penti_shift_chord_map[] __attribute__ ((section (".keymap.keymaps"))) = {
#else
static const penti_chord_map_entry_t penti_shift_chord_map[] PROGMEM = {
#endif
    { .key_code = KC_NO },     // 0x00
    { .key_code = KC_SPACE },
    { .key_code = KC_S, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_F, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_E, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_R, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_L, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_Z, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_I, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_A, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_C, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_Q, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_O, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_B, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_U, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_P, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_N, .modifiers = MOD_BIT(KC_LSHIFT) },      // 0x10
    { .key_code = KC_D, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_J, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_H, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_G, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_Y, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_V, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_X, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_M, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_T, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_K, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_W, .modifiers = MOD_BIT(KC_LSHIFT) },
};

#ifdef KEYMAP_SECTION_ENABLE
static const penti_chord_map_entry_t penti_punct_chord_map[] __attribute__ ((section (".keymap.keymaps"))) = {
#else
static const penti_chord_map_entry_t penti_punct_chord_map[] PROGMEM = {
#endif
    { .key_code = KC_NO },     // 0x00
    { .key_code = KC_SPACE },
    { .key_code = KC_8,        .modifiers = MOD_BIT(KC_LSHIFT) }, // *
    { .key_code = KC_SLASH,    .modifiers = MOD_BIT(KC_LSHIFT) }, // ?
    { .key_code = KC_LBRACKET }, // [
    { .key_code = KC_4,        .modifiers = MOD_BIT(KC_LSHIFT) }, // $
    { .key_code = KC_MINUS,    .modifiers = MOD_BIT(KC_LSHIFT) }, // _
    { .key_code = KC_QUOTE,    .modifiers = MOD_BIT(KC_LSHIFT) }, // "
    { .key_code = KC_1,        .modifiers = MOD_BIT(KC_LSHIFT) }, // !
    { .key_code = KC_GRAVE },    // `
    { .key_code = KC_RBRACKET }, // ]
    { .key_code = KC_QUOTE },    // '
    { .key_code = KC_BSLASH,   .modifiers = MOD_BIT(KC_LSHIFT) }, // |
    { .key_code = KC_LBRACKET, .modifiers = MOD_BIT(KC_LSHIFT) }, // {
    { .key_code = KC_7,        .modifiers = MOD_BIT(KC_LSHIFT) }, // &
    { .key_code = KC_RBRACKET, .modifiers = MOD_BIT(KC_LSHIFT) }, // }
    { .key_code = KC_0,        .modifiers = MOD_BIT(KC_LSHIFT) }, // 0x10, )
    { .key_code = KC_SLASH },    // /
    { .key_code = KC_SCOLON },   // ;
    { .key_code = KC_3,        .modifiers = MOD_BIT(KC_LSHIFT) }, // #
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_EQUAL },    // =
    { .key_code = KC_6,        .modifiers = MOD_BIT(KC_LSHIFT) }, // ^
    { .key_code = KC_9,        .modifiers = MOD_BIT(KC_LSHIFT) }, // (
    { .key_code = KC_BSLASH },   // '\'
    { .key_code = KC_DOT,      .modifiers = MOD_BIT(KC_LSHIFT) }, // >
    { .key_code = KC_5,        .modifiers = MOD_BIT(KC_LSHIFT) }, // %
    { .key_code = KC_2,        .modifiers = MOD_BIT(KC_LSHIFT) }, // @
    { .key_code = KC_COMMA,    .modifiers = MOD_BIT(KC_LSHIFT) }, // <
};

#ifdef KEYMAP_SECTION_ENABLE
static const penti_chord_map_entry_t penti_digit_chord_map[] __attribute__ ((section (".keymap.keymaps"))) = {
#else
static const penti_chord_map_entry_t penti_digit_chord_map[] PROGMEM = {
#endif
    { .key_code = KC_NO },     // 0x00
    { .key_code = KC_SPACE },
    { .key_code = KC_1 },
    { .key_code = KC_4 },
    { .key_code = KC_2 },
    { .key_code = KC_5 },
    { .key_code = KC_7 },
    { .key_code = KC_END },
    { .key_code = KC_3 },
    { .key_code = KC_6 },
    { .key_code = KC_COMMA },
    { .key_code = KC_PGUP },
    { .key_code = KC_8 },
    { .key_code = KC_LEFT },
    { .key_code = KC_0 },
    { .key_code = KC_EQUAL, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_RIGHT },      // 0x10
    { .key_code = KC_DOT },
    { .key_code = KC_SCOLON, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_HOME },
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_9 },
    { .key_code = KC_UP },
    { .key_code = KC_DOWN },
    { .key_code = KC_PGDOWN },
    { .key_code = KC_MINUS },
    { .key_code = KC_GRAVE, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_NO },     // <empty>
    { .key_code = KC_INSERT },
};

#ifdef KEYMAP_SECTION_ENABLE
static const penti_chord_map_entry_t penti_funct_chord_map[] __attribute__ ((section (".keymap.keymaps"))) = {
#else
static const penti_chord_map_entry_t penti_funct_chord_map[] PROGMEM = {
#endif
    { .key_code = KC_NO },     // 0x00
    { .key_code = KC_NO },     // NEW
    { .key_code = KC_F1 },
    { .key_code = KC_F4 },
    { .key_code = KC_F2 },
    { .key_code = KC_F5 },
    { .key_code = KC_F7 },
    { .key_code = KC_F11 },
    { .key_code = KC_F3 },
    { .key_code = KC_F6 },
    { .key_code = KC_NO },     // COPY
    { .key_code = KC_NO },     // QUIT
    { .key_code = KC_F8 },
    { .key_code = KC_PAUSE },  // BREAK
    { .key_code = KC_F10 },
    { .key_code = KC_NO },     // PASTE
    { .key_code = KC_NO },     // 0x10, NUM
    { .key_code = KC_NO },     // DEF
    { .key_code = KC_NO },     // <empty>
    { .key_code = KC_NO },     // HELP
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_NO },
    { .key_code = KC_F9 },
    { .key_code = KC_NO },     // <empty>
    { .key_code = KC_NO },     // <empty>
    { .key_code = KC_NO },     // PASTE2
    { .key_code = KC_F12 },
    { .key_code = KC_NO },     // <empty>
    { .key_code = KC_NO },     // RESET
    { .key_code = KC_NO },     // <empty>
};

enum function_id {
    D_MACRO_FUNC_RECORD,
    D_MACRO_FUNC_PLAY,
    D_MACRO_FUNC_PLAY_1,
    D_MACRO_FUNC_PLAY_2,
    FUNC_LSHIFT_LPAREN,
    FUNC_RSHIFT_RPAREN,
    PENTI_KEY,
};

typedef enum {
    PENTI_THUMB_BIT = 0,
    PENTI_INDEX_BIT,
    PENTI_MIDDLE_BIT,
    PENTI_RING_BIT,
    PENTI_PINKY_BIT,
    PENTI_REPEAT_BIT,
} penti_bit_t;

#define AC_L1      ACTION_LAYER_TAP_TOGGLE(1)
#define AC_L2      ACTION_LAYER_TAP_KEY(2, KC_SPC)
#define AC_CTLENT  ACTION_MODS_TAP_KEY(MOD_RCTL, KC_ENT)
#define AC_MREC    ACTION_FUNCTION_TAP(D_MACRO_FUNC_RECORD)
#define AC_MPLAY   ACTION_FUNCTION_TAP(D_MACRO_FUNC_PLAY)
#define AC_MPLAY_1 ACTION_FUNCTION_TAP(D_MACRO_FUNC_PLAY_1)
#define AC_MPLAY_2 ACTION_FUNCTION_TAP(D_MACRO_FUNC_PLAY_2)
#define AC_LOCK    ACTION_LAYER_TAP_TOGGLE(3)
#define AC_UNLOCK  ACTION_LAYER_MOMENTARY(4)
#define AC_LSFTPRN ACTION_FUNCTION_TAP(FUNC_LSHIFT_LPAREN)
#define AC_RSFTPRN ACTION_FUNCTION_TAP(FUNC_RSHIFT_RPAREN)

#define AC_PENTI        ACTION_LAYER_TOGGLE(5)
#define AC_PENTI_THUMB  ACTION_FUNCTION_OPT(PENTI_KEY, PENTI_THUMB_BIT)
#define AC_PENTI_INDEX  ACTION_FUNCTION_OPT(PENTI_KEY, PENTI_INDEX_BIT)
#define AC_PENTI_MIDDLE ACTION_FUNCTION_OPT(PENTI_KEY, PENTI_MIDDLE_BIT)
#define AC_PENTI_RING   ACTION_FUNCTION_OPT(PENTI_KEY, PENTI_RING_BIT)
#define AC_PENTI_PINKY  ACTION_FUNCTION_OPT(PENTI_KEY, PENTI_PINKY_BIT)
#define AC_PENTI_REPEAT ACTION_FUNCTION_OPT(PENTI_KEY, PENTI_REPEAT_BIT)


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
          MPLAY_1, MPLAY_2,       SPC,                    MREC, MPLAY),

    /* layer 2: vi movement keys and mouse keys (space) */
    [2] = UNIMAP_HHKB(
    PENTI, F1,   F2,   F3,   F4,   F5,   F6,   F7,   F8,   F9,   F10,  F11,  F12,    INS,  DEL,
    TAB,   TRNS, WH_L, WH_U, WH_D, WH_R, HOME, PGDN, PGUP, END,  TRNS, TRNS, TRNS,   BSPC,
    LCTL,  TRNS, MS_L, MS_U, MS_D, MS_R, LEFT, DOWN, UP,   RGHT, TRNS, TRNS, CTLENT,
    LSFT,  TRNS, BTN3, BTN2, BTN1, TRNS, ACL2, ACL1, ACL0, TRNS, TRNS, RSFT, TRNS,
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

    /* layer 5: Penti keyboard mode */
    [5] = UNIMAP_HHKB(
    PENTI, NO,          NO,           NO,           NO,          NO, NO, NO, NO, NO, NO, NO, NO, NO, NO,
    NO,    NO,          NO,           NO,           NO,          NO, NO, NO, NO, NO, NO, NO, NO, NO,
    NO,    PENTI_PINKY, PENTI_RING,   PENTI_MIDDLE, PENTI_INDEX, NO, NO, NO, NO, NO, NO, NO, NO,
    NO,    NO,          PENTI_REPEAT, NO,           NO,          NO, NO, NO, NO, NO, NO, NO, NO,
           NO,          NO,                                      PENTI_THUMB,        NO, NO),
};


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


#define PENTI_KEYS_COUNT 6

typedef struct {
    uint8_t bit;
    uint8_t pressed;
    uint16_t time;
} penti_event_t;

#define PENTI_CHORD_MAP_STACK_SIZE 7

typedef struct {
    const penti_chord_map_entry_t *map;
    uint8_t transient;
} penti_chord_map_stack_entry_t;

typedef struct {
    uint8_t keys_state;
    uint8_t keys_combo;
    uint8_t event_count;
    penti_event_t event_list[PENTI_KEYS_COUNT];
    uint8_t extra_modifiers;
    uint8_t extra_modifiers_transient;
    int8_t chord_map_stack_top;
    penti_chord_map_stack_entry_t chord_map_stack[PENTI_CHORD_MAP_STACK_SIZE];
    penti_chord_map_entry_t to_repeat;
} penti_state_t;

static penti_state_t penti_state = {
    .keys_state = 0,
    .keys_combo = 0,
    .event_count = 0,
    .extra_modifiers = 0,
    .extra_modifiers_transient = 0,
    .chord_map_stack_top = 0,
    .chord_map_stack = { { .map = penti_alpha_chord_map, .transient = 0 } },
    .to_repeat = { .key_code = KC_NO },
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

static void action_play(keyrecord_t *record, int16_t interval)
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

static void penti_tap_hw_key(enum hid_keyboard_keypad_usage key_code, uint8_t modifiers)
{
    if (modifiers > 0) {
        add_weak_mods(modifiers);
        send_keyboard_report();
    }

    // simulate a tap
    register_code(key_code);
    unregister_code(key_code);

    if (modifiers > 0) {
        del_weak_mods(modifiers);
    }
    send_keyboard_report();
}

static penti_chord_map_stack_entry_t *get_chord_map(void)
{
    return &(penti_state.chord_map_stack[penti_state.chord_map_stack_top]);
}

static void push_chord_map(const penti_chord_map_entry_t *map, uint8_t transient)
{
    dprintf("Before push_chord_map: chord_map_stack_top = %d\n",
            penti_state.chord_map_stack_top);

    if (penti_state.chord_map_stack_top >= 0) {
        uint8_t move = 0;
        for (uint8_t i = 0; i <= penti_state.chord_map_stack_top; i++) {
            if (penti_state.chord_map_stack[i].map == map) {
                move = 1;
            }
            if (move && i < penti_state.chord_map_stack_top) {
                penti_state.chord_map_stack[i] = penti_state.chord_map_stack[i+1];
            }
        }
        if (move) {
            penti_state.chord_map_stack_top--;
        }
    }

    penti_state.chord_map_stack_top++;
    penti_state.chord_map_stack[penti_state.chord_map_stack_top].map = map;
    penti_state.chord_map_stack[penti_state.chord_map_stack_top].transient = transient;

    dprintf("After push_chord_map: chord_map_stack_top = %d\n",
            penti_state.chord_map_stack_top);
}

static penti_chord_map_stack_entry_t *pop_chord_map(void)
{
    dprintf("Before pop_chord_map: chord_map_stack_top = %d\n",
            penti_state.chord_map_stack_top);

    if (penti_state.chord_map_stack_top <= 0) {
        return NULL;
    }
    penti_chord_map_stack_entry_t *cur_map = get_chord_map();
    penti_state.chord_map_stack_top--;

    dprintf("After pop_chord_map: chord_map_stack_top = %d\n",
            penti_state.chord_map_stack_top);

    return cur_map;
}

static void penti_clear_transient_modifiers(void)
{
    penti_state.extra_modifiers &= (~(penti_state.extra_modifiers_transient));
    penti_state.extra_modifiers_transient = 0;
}

static void handle_penti_repeat_key(uint8_t down)
{
    if (penti_state.to_repeat.key_code == KC_NO) {
        return;
    }

    uint8_t modifiers =
        penti_state.to_repeat.modifiers | penti_state.extra_modifiers;

    if (down) {
        if (modifiers > 0) {
            register_mods(modifiers);
        }
        register_code(penti_state.to_repeat.key_code);
    } else {
        unregister_code(penti_state.to_repeat.key_code);
        if (modifiers > 0) {
            unregister_mods(modifiers);
            penti_clear_transient_modifiers();
        }
    }
}

static void handle_penti_chord(uint8_t combo)
{
    penti_chord_map_stack_entry_t *stack_entry = get_chord_map();

#if defined(__AVR__)
    uint16_t entry_val = pgm_read_word(&(stack_entry->map[combo]));
    penti_chord_map_entry_t entry = {
        .key_code = (uint8_t)((entry_val) & 0x00ff),
        .modifiers = (uint8_t)((entry_val >> 8) & 0x00ff)
    };
#else
    penti_chord_map_entry_t entry = stack_entry->map[combo];
#endif

    if (entry.key_code != KC_NO) {
        uint8_t modifiers = entry.modifiers | penti_state.extra_modifiers;

        dprintf("Penti chord: combo = %02X, modifiers = %02X\n",
                combo, modifiers);

        penti_state.to_repeat = entry;
        // merge modifiers to retain full input state
        penti_state.to_repeat.modifiers = modifiers;

        penti_tap_hw_key(entry.key_code, modifiers);

        if (stack_entry->transient) {
            pop_chord_map();
        }
        penti_clear_transient_modifiers();
    } else {
        while (pop_chord_map());
    }
}

static void handle_penti_arpeggio(uint8_t combo, uint8_t ev_count, penti_event_t ev_list[])
{
    switch (combo) {

        case ((1 << PENTI_THUMB_BIT) | (1 << PENTI_INDEX_BIT)):
            switch (ev_list[0].bit) {
                case PENTI_THUMB_BIT:
                    push_chord_map(penti_shift_chord_map, 1);
                    break;
                case PENTI_INDEX_BIT:
                    if ((get_chord_map())->map == penti_shift_chord_map) {
                        pop_chord_map();
                    } else {
                        push_chord_map(penti_shift_chord_map, 0);
                    }
                    break;
            }
            break;

        case ((1 << PENTI_THUMB_BIT) | (1 << PENTI_MIDDLE_BIT)):
            switch (ev_list[0].bit) {
                case PENTI_THUMB_BIT:
                    push_chord_map(penti_punct_chord_map, 1);
                    break;
                case PENTI_MIDDLE_BIT:
                    if ((get_chord_map())->map == penti_punct_chord_map) {
                        pop_chord_map();
                    } else {
                        push_chord_map(penti_punct_chord_map, 0);
                    }
                    break;
            }
            break;

        case ((1 << PENTI_THUMB_BIT) | (1 << PENTI_RING_BIT)):
            switch (ev_list[0].bit) {
                case PENTI_THUMB_BIT:
                    push_chord_map(penti_digit_chord_map, 1);
                    break;
                case PENTI_RING_BIT:
                    if ((get_chord_map())->map == penti_digit_chord_map) {
                        pop_chord_map();
                    } else {
                        push_chord_map(penti_digit_chord_map, 0);
                    }
                    break;
            }
            break;

        case ((1 << PENTI_MIDDLE_BIT) | (1 << PENTI_RING_BIT)):
            switch (ev_list[0].bit) {
                case PENTI_MIDDLE_BIT:
                    push_chord_map(penti_funct_chord_map, 1);
                    break;
                case PENTI_RING_BIT:
                    if ((get_chord_map())->map == penti_funct_chord_map) {
                        pop_chord_map();
                    } else {
                        push_chord_map(penti_funct_chord_map, 0);
                    }
                    break;
            }
            break;

        case ((1 << PENTI_THUMB_BIT) | (1 << PENTI_PINKY_BIT)):
            switch (ev_list[0].bit) {
                case PENTI_THUMB_BIT:
                    penti_state.extra_modifiers |= MOD_BIT(KC_LCTRL);
                    penti_state.extra_modifiers_transient |= MOD_BIT(KC_LCTRL);
                    break;
                case PENTI_PINKY_BIT:
                    penti_state.extra_modifiers_transient &= (~(MOD_BIT(KC_LCTRL)));
                    if (penti_state.extra_modifiers & MOD_BIT(KC_LCTRL)) {
                        penti_state.extra_modifiers &= (~(MOD_BIT(KC_LCTRL)));
                    } else {
                        penti_state.extra_modifiers |= MOD_BIT(KC_LCTRL);
                    }
                    break;
            }
            break;

        case ((1 << PENTI_INDEX_BIT) | (1 << PENTI_RING_BIT)):
            switch (ev_list[0].bit) {
                case PENTI_INDEX_BIT:
                    penti_tap_hw_key(KC_ENTER, penti_state.extra_modifiers);
                    penti_clear_transient_modifiers();
                    penti_state.to_repeat.key_code = KC_ENTER;
                    penti_state.to_repeat.modifiers = 0;
                    break;
                case PENTI_RING_BIT:
                    penti_tap_hw_key(KC_ESCAPE, penti_state.extra_modifiers);
                    penti_clear_transient_modifiers();
                    penti_state.to_repeat.key_code = KC_ESCAPE;
                    penti_state.to_repeat.modifiers = 0;
                    break;
            }
            break;

        case ((1 << PENTI_INDEX_BIT) | (1 << PENTI_MIDDLE_BIT)):
            switch (ev_list[0].bit) {
                case PENTI_INDEX_BIT:
                    penti_tap_hw_key(KC_TAB, penti_state.extra_modifiers);
                    penti_clear_transient_modifiers();
                    penti_state.to_repeat.key_code = KC_TAB;
                    penti_state.to_repeat.modifiers = 0;
                    break;
                case PENTI_MIDDLE_BIT:
                    penti_tap_hw_key(KC_BSPACE, penti_state.extra_modifiers);
                    penti_clear_transient_modifiers();
                    penti_state.to_repeat.key_code = KC_BSPACE;
                    penti_state.to_repeat.modifiers = 0;
                    break;
            }
            break;

        default:
            return;
    }
}

static void action_penti_key(keyrecord_t *record, uint8_t bit)
{
    dprintf("Penti key event: bit %u, %s [%u]\n",
            bit, record->event.pressed ? "down" : "up",
            record->event.time);

    if (bit == PENTI_REPEAT_BIT) {
        handle_penti_repeat_key(record->event.pressed);
        return;
    }

    uint8_t mask = (1 << bit);

    if (record->event.pressed) {
        if (penti_state.event_count >= PENTI_KEYS_COUNT) {
            dprintln("  XXX: Penti event list overflow");
            penti_state.keys_state = 0;
            penti_state.keys_combo = 0;
            penti_state.event_count = 0;
        } else {
            penti_state.keys_state |= mask;
            penti_state.keys_combo |= mask;

            penti_event_t *penti_ev = &(penti_state.event_list[penti_state.event_count++]);
            penti_ev->bit = bit;
            penti_ev->pressed = record->event.pressed;
            penti_ev->time = record->event.time;
        }
    } else {
        penti_state.keys_state &= (~mask);
        if (penti_state.keys_state == 0) {
            dprintf("Penti combo engaged: 0x%02X, event count: %u\n",
                    penti_state.keys_combo,
                    penti_state.event_count);

            if (penti_state.event_count > 1) {
                penti_event_t *last = &(penti_state.event_list[penti_state.event_count - 1]),
                              *second_to_last = &(penti_state.event_list[penti_state.event_count - 2]);
                if (TIMER_DIFF_16(record->event.time, last->time) <= 240 &&
                    TIMER_DIFF_16(last->time, second_to_last->time) >= 80) {
                    dprintln("  Penti arpeggio");
                    handle_penti_arpeggio(penti_state.keys_combo,
                                          penti_state.event_count,
                                          penti_state.event_list);
                } else {
                    dprintln("  Penti chord");
                    handle_penti_chord(penti_state.keys_combo);
                }
            } else {
                dprintln("  Penti chord");
                handle_penti_chord(penti_state.keys_combo);
            }

            penti_state.keys_combo = 0;
            penti_state.event_count = 0;
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
            action_play(record, -1);
            break;
        case D_MACRO_FUNC_PLAY_1:
            action_play(record, 1);
            break;
        case D_MACRO_FUNC_PLAY_2:
            action_play(record, 50);
            break;
        case FUNC_LSHIFT_LPAREN:
            action_shift_paren(record, KC_LSHIFT);
            break;
        case FUNC_RSHIFT_RPAREN:
            action_shift_paren(record, KC_RSHIFT);
            break;
        case PENTI_KEY:
            action_penti_key(record, opt);
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
