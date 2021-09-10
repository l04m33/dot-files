/*
 * Copyright (c) 2018 Charlie Waters
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * l04m33 custom keymap, based on default Vortex keymap.c found
 * in qmk_pok3r.
 */

#include <print.h>

#include "vortex.h"

enum keycodes {
    DUMMY = SAFE_RANGE,
    DYNAMIC_MACRO_RANGE,
};

#include "dynamic_macro.h"

#include "penti.h"
#include "shift_paren.h"


enum function_id {
    FUNC_LSHIFT_LPAREN,
    FUNC_RSHIFT_RPAREN,
    FUNC_AUTO_LBRACKET,
    FUNC_AUTO_RBRACKET,
    FUNC_AUTO_PAREN,
    FUNC_PENTI_KEY,
};

enum layer_id {
    LAYER_COLEMAK_DH = 0,
    LAYER_QWERTY,
    LAYER_SPCFN,
    LAYER_SWITCHES,
    LAYER_NP,
    LAYER_PENTI,

    LAYER_COUNT,
};

#define AC_CTLESC    MT(MOD_LCTL, KC_ESC)
#define AC_CTLENT    MT(MOD_RCTL, KC_ENT)
#define AC_ALTBSP    MT(MOD_LALT, KC_BSPC)
#define AC_SPCFN     LT(LAYER_SPCFN, KC_SPC)
#define AC_TABNP     LT(LAYER_NP, KC_TAB)

#define AC_SWTS      MO(LAYER_SWITCHES)
#define AC_QWERTY    TG(LAYER_QWERTY)
#define AC_SPCFNT    TG(LAYER_SPCFN)

#define AC_LSFTPRN   F(6)
#define AC_RSFTPRN   F(7)
#define AC_AUTOLBRC  F(8)
#define AC_AUTORBRC  F(9)
#define AC_AUTOPRN   F(10)

#define AC_PENTI        TG(LAYER_PENTI)
#define AC_PENTI_THUMB  F(0)
#define AC_PENTI_INDEX  F(1)
#define AC_PENTI_MIDDLE F(2)
#define AC_PENTI_RING   F(3)
#define AC_PENTI_PINKY  F(4)
#define AC_PENTI_REPEAT F(5)

/************************************************
 * keymaps
 ************************************************/

const uint16_t keymaps_default[][MATRIX_ROWS][MATRIX_COLS] = {
#if defined(KEYMAP_VORTEX_CORE)

    [LAYER_COLEMAK_DH] = LAYOUT_core(
        AC_TABNP,   KC_Q,    KC_W,    KC_F,      KC_P,     KC_B,     KC_J,    KC_L,    KC_U,    KC_Y,    KC_SCLN,    AC_AUTOLBRC, AC_AUTORBRC,
        AC_CTLESC,  KC_A,    KC_R,    KC_S,      KC_T,     KC_G,     KC_M,    KC_N,    KC_E,    KC_I,    KC_O,       AC_CTLENT,
        AC_LSFTPRN, KC_X,    KC_C,    KC_D,      KC_V,     KC_Z,     KC_K,    KC_H,    KC_COMM, KC_DOT,  AC_RSFTPRN, XXXXXXX,
        XXXXXXX,    AC_SWTS, KC_LGUI, AC_ALTBSP, AC_SPCFN, AC_SPCFN, KC_RALT, KC_RGUI, AC_SWTS, XXXXXXX
    ),

    [LAYER_QWERTY] = LAYOUT_core(
        _______,    KC_Q,    KC_W,    KC_E,      KC_R,     KC_T,     KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,       _______, _______,
        _______,    KC_A,    KC_S,    KC_D,      KC_F,     KC_G,     KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN,    _______,
        _______,    KC_Z,    KC_X,    KC_C,      KC_V,     KC_B,     KC_N,    KC_M,    _______, _______, _______,    _______,
        _______,    _______, _______, _______,   _______,  _______,  _______, _______, _______, _______
    ),

    [LAYER_SPCFN] = LAYOUT_core(
        KC_GRV,     KC_EQL,  KC_3,    KC_2,      KC_1,     KC_MINS,  KC_HOME, KC_PGDN, KC_PGUP, KC_END,  KC_DEL,     KC_LBRC, KC_RBRC,
        KC_LCTL,    KC_0,    KC_6,    KC_5,      KC_4,     KC_QUOT,  KC_LEFT, KC_DOWN, KC_UP,   KC_RIGHT,KC_INS,     KC_RCTL,
        KC_LSFT,    KC_9,    KC_8,    KC_7,      KC_DOT,   KC_ENT,   KC_RBRC, KC_BSLS, _______, _______, KC_SLSH,    _______,
        _______,    _______, _______, _______,   _______,  _______,  _______, _______, _______, _______
    ),

    [LAYER_NP] = LAYOUT_core(
        AC_TABNP,   KC_PPLS, KC_P3,   KC_P2,     KC_P1,    KC_PMNS,  KC_NLCK, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,    XXXXXXX, XXXXXXX,
        _______,    KC_P0,   KC_P6,   KC_P5,     KC_P4,    KC_PSLS,  KC_PAST, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,    _______,
        _______,    KC_P9,   KC_P8,   KC_P7,     KC_PDOT,  KC_PENT,  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, _______,    _______,
        _______,    _______, _______, _______,   _______,  _______,  _______, _______, _______, _______
    ),

    /* XXX: mouse keys are not working due to lack of endpoints (?) */
    [LAYER_SWITCHES] = LAYOUT_core(
        AC_PENTI,   KC_F11,  KC_F3,   KC_F2,     KC_F1,    KC_PSCR,  AC_QWERTY, AC_SPCFNT, AC_AUTOPRN, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
        _______,    KC_F10,  KC_F6,   KC_F5,     KC_F4,    KC_CAPS,  KC_MS_L, KC_MS_D, KC_MS_U, KC_MS_R, KC_MS_BTN1, KC_MS_BTN2,
        _______,    KC_F9,   KC_F8,   KC_F7,     KC_F12,   KC_SLCK,  DYN_REC_START1, DYN_REC_STOP, DYN_MACRO_PLAY1, XXXXXXX, _______, _______,
        _______,    _______, _______, _______,   _______,  _______,  _______, _______, _______, _______
    ),

    [LAYER_PENTI] = LAYOUT_core(
        AC_PENTI, XXXXXXX,        AC_PENTI_RING, AC_PENTI_MIDDLE, AC_PENTI_INDEX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
        XXXXXXX,  AC_PENTI_PINKY, XXXXXXX,       AC_PENTI_REPEAT, XXXXXXX,        XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
        XXXXXXX,  XXXXXXX,        XXXXXXX,       XXXXXXX,         XXXXXXX,        XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
        XXXXXXX,  XXXXXXX,        XXXXXXX,       XXXXXXX,         AC_PENTI_THUMB, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX
    ),
#else
    #error "No Keymap!"
#endif
};
const uint16_t keymaps_default_size = sizeof(keymaps_default);
uint16_t keymaps[MAX_LAYERS][MATRIX_ROWS][MATRIX_COLS];

/************************************************
 * keymaps end
 ************************************************/

const uint16_t PROGMEM fn_actions[] = {
    [0] = ACTION_FUNCTION_OPT(FUNC_PENTI_KEY, PENTI_THUMB_BIT),
    [1] = ACTION_FUNCTION_OPT(FUNC_PENTI_KEY, PENTI_INDEX_BIT),
    [2] = ACTION_FUNCTION_OPT(FUNC_PENTI_KEY, PENTI_MIDDLE_BIT),
    [3] = ACTION_FUNCTION_OPT(FUNC_PENTI_KEY, PENTI_RING_BIT),
    [4] = ACTION_FUNCTION_OPT(FUNC_PENTI_KEY, PENTI_PINKY_BIT),
    [5] = ACTION_FUNCTION_OPT(FUNC_PENTI_KEY, PENTI_REPEAT_BIT),

    [6] = ACTION_FUNCTION_TAP(FUNC_LSHIFT_LPAREN),
    [7] = ACTION_FUNCTION_TAP(FUNC_RSHIFT_RPAREN),
    [8] = ACTION_FUNCTION_TAP(FUNC_AUTO_LBRACKET),
    [9] = ACTION_FUNCTION_TAP(FUNC_AUTO_RBRACKET),
    [10] = ACTION_FUNCTION_TAP(FUNC_AUTO_PAREN),
};

const macro_t *action_get_macro(keyrecord_t *record, uint8_t id, uint8_t opt) {
    return MACRO_NONE;
};


void matrix_init_user(void) {
    auto_paren_state.enabled = 1;
}

void matrix_scan_user(void) {

}

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    if (!process_record_dynamic_macro(keycode, record)) {
        return false;
    }
    return true;
}

void led_set_user(uint8_t usb_led) {

}

void action_function(keyrecord_t *record, uint8_t id, uint8_t opt)
{
    switch (id) {
        case FUNC_PENTI_KEY:
            action_penti_key(record, opt);
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
        default:
            break;
    }
}
