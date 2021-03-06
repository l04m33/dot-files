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
#include "eeconfig.h"
#include "bootmagic.h"
#include "unimap_trans.h"
#if defined(__AVR__)
#   include <avr/pgmspace.h>
#endif


#include "penti.h"
#include "shift_paren.h"
#include "d_macro.h"


enum function_id {
    D_MACRO_FUNC_RECORD,
    D_MACRO_FUNC_PLAY,
    D_MACRO_FUNC_PLAY_1,
    D_MACRO_FUNC_PLAY_2,
    FUNC_LSHIFT_LPAREN,
    FUNC_RSHIFT_RPAREN,
    FUNC_AUTO_LBRACKET,
    FUNC_AUTO_RBRACKET,
    FUNC_AUTO_PAREN,
    PENTI_KEY,
};

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
#define AC_AUTOLBRC ACTION_FUNCTION_TAP(FUNC_AUTO_LBRACKET)
#define AC_AUTORBRC ACTION_FUNCTION_TAP(FUNC_AUTO_RBRACKET)
#define AC_AUTOPRN ACTION_FUNCTION_TAP(FUNC_AUTO_PAREN)

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
    ESC,     1,    2,    3,   4,   5,   6,   7,   8,    9,    0,    MINS,     EQL,      BSLS, GRV,
    TAB,     Q,    W,    E,   R,   T,   Y,   U,   I,    O,    P,    AUTOLBRC, AUTORBRC, BSPC,
    LCTL,    A,    S,    D,   F,   G,   H,   J,   K,    L,    SCLN, QUOT,     CTLENT,
    LSFTPRN, Z,    X,    C,   V,   B,   N,   M,   COMM, DOT,  SLSH, RSFTPRN,  L1,
             LGUI, LALT,           L2,                  RALT, RGUI),

    /* layer 1: hhkb mode (hhkb fn) */
    [1] = UNIMAP_HHKB(
    PWR,  F1,   F2,   F3,   F4,   F5,   F6,   F7,   F8,   F9,   F10,  F11,  F12,  INS,  DEL,
    CAPS, TRNS, TRNS, TRNS, TRNS, TRNS, TRNS, TRNS, PSCR, SLCK, PAUS, UP,   TRNS, BSPC,
    TRNS, VOLD, VOLU, MUTE, TRNS, TRNS, PAST, PSLS, HOME, PGUP, LEFT, RGHT, PENT,
    LOCK, TRNS, TRNS, TRNS, TRNS, TRNS, PPLS, PMNS, END,  PGDN, DOWN, AUTOPRN, L1,
          MPLAY_1, MPLAY_2,       SPC,                    MREC, MPLAY),

    /* layer 2: vi movement keys and mouse keys (space) */
    [2] = UNIMAP_HHKB(
    PENTI, F1,   F2,   F3,   F4,   F5,   F6,   F7,   F8,   F9,   F10,  F11,  F12,    INS,  DEL,
    TAB,   TRNS, WH_L, WH_U, WH_D, WH_R, HOME, PGDN, PGUP, END,  TRNS, LBRC, RBRC,   BSPC,
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
    NO,    NO,          PENTI_RING,   PENTI_MIDDLE, PENTI_INDEX, NO, NO, NO, NO, NO, NO, NO, NO, NO,
    NO,    PENTI_PINKY, NO,           PENTI_REPEAT, NO,          NO, NO, NO, NO, NO, NO, NO, NO,
    NO,    NO,          NO,           NO,           NO,          NO, NO, NO, NO, NO, NO, NO, NO,
           NO,          NO,                                      PENTI_THUMB,        NO, NO),
};


void action_function(keyrecord_t *record, uint8_t id, uint8_t opt)
{
    switch (id) {
        case D_MACRO_FUNC_RECORD:
            d_macro_action_record(record);
            break;
        case D_MACRO_FUNC_PLAY:
            d_macro_action_play(record, -1);
            break;
        case D_MACRO_FUNC_PLAY_1:
            d_macro_action_play(record, 1);
            break;
        case D_MACRO_FUNC_PLAY_2:
            d_macro_action_play(record, 50);
            break;
        case FUNC_LSHIFT_LPAREN:
            action_shift_paren(record, KC_LSHIFT);
            break;
        case FUNC_RSHIFT_RPAREN:
            action_shift_paren(record, KC_RSHIFT);
            break;
        case FUNC_AUTO_LBRACKET:
            action_shift_paren(record, KC_LBRACKET);
            break;
        case FUNC_AUTO_RBRACKET:
            action_shift_paren(record, KC_RBRACKET);
            break;
        case FUNC_AUTO_PAREN:
            action_auto_paren(record);
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
    d_macro_hook_matrix_change(event);
}

void hook_keyboard_loop(void)
{
    d_macro_stack_scan();
    d_macro_hook_keyboard_loop();
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

    return d_macro_action_for_key(key, action);
}


void hook_late_init(void)
{
    d_macro_stack_scan_hook_late_init();
}
