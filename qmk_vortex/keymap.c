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

/************************************************
 * keymaps
 ************************************************/

const uint16_t keymaps_default[][MATRIX_ROWS][MATRIX_COLS] = {
#if defined(KEYMAP_VORTEX_CORE)
    /* default layer */
    [0] = LAYOUT_core(
        KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,         KC_T,         KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    F(8), F(9),
        KC_LCTL, KC_A,    KC_S,    KC_D,    KC_F,         KC_G,         KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, MT(MOD_RCTL,KC_ENT),
        F(6),    KC_Z,    KC_X,    KC_C,    KC_V,         KC_B,         KC_N,    KC_M,    KC_COMM, KC_DOT,  F(7),    MO(2),
        KC_ESC,  KC_LGUI, KC_LALT, KC_BSPC, LT(3,KC_SPC), LT(3,KC_SPC), MO(1),   KC_RALT, KC_APP,  KC_RCTL
    ),
    /* Fn layer */
    [1] = LAYOUT_core(
        DEBUG,   KC_VOLD, KC_VOLU, KC_MUTE, _______, _______, _______, KC_PGUP, KC_UP,   KC_PGDN, KC_PSCR, KC_SLCK, KC_PAUS,
        KC_CAPS, KC_MPRV, KC_MPLY, KC_MNXT, _______, _______, KC_HOME, KC_LEFT, KC_DOWN, KC_RIGHT,KC_INS,  KC_ENT,
        KC_LSFT, _______, _______, _______, _______, _______, KC_END,  _______, _______, _______, KC_RSFT, _______,
        _______, _______, _______, KC_DEL,  _______, _______, _______, _______, _______, _______
    ),
    /* Fn1 layer */
    [2] = LAYOUT_core(
        KC_GRV,          KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   KC_F6,          KC_F7,        KC_F8,   KC_F9,   KC_F10,  KC_F11, KC_F12,
        KC_1,            KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,           KC_8,         KC_9,    KC_0,    KC_MINS, KC_EQL,
        _______,         _______, _______, _______, _______, KC_QUOT, KC_SLSH,        KC_LBRC,      KC_RBRC, KC_BSLS, F(10),    _______,
        DYN_MACRO_PLAY1, _______, _______, _______, _______, _______, DYN_REC_START1, DYN_REC_STOP, _______, _______
    ),
    /* Space Fn layer */
    /* XXX: mouse keys are not working due to lack of endpoints (?) */
    [3] = LAYOUT_core(
        TG(4),   _______, KC_WH_L, KC_WH_U, KC_WH_D, KC_WH_R, KC_HOME, KC_PGDN, KC_PGUP, KC_END,  _______, KC_LBRC, KC_RBRC,
        _______, _______, KC_MS_L, KC_MS_U, KC_MS_D, KC_MS_R, KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT, _______, KC_ENT,
        KC_LSFT, _______, KC_BTN3, KC_BTN2, KC_BTN1, _______, KC_ACL2, KC_ACL1, KC_ACL0, _______, KC_RSFT, _______,
        _______, _______, _______, _______, _______, _______, _______, _______, _______, _______
    ),
    /* Penti mode layer */
    [4] = LAYOUT_core(
        TG(4),   XXXXXXX, F(3),    F(2),    F(1),    XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
        XXXXXXX, F(4),    XXXXXXX, F(5),    XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
        XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
        XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, F(0),    XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX
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

enum function_id {
    PENTI_KEY,
    LSHIFT_LPAREN,
    RSHIFT_RPAREN,
    AUTO_LBRACKET,
    AUTO_RBRACKET,
    AUTO_PAREN,
};

const uint16_t PROGMEM fn_actions[] = {
    [0] = ACTION_FUNCTION_OPT(PENTI_KEY, PENTI_THUMB_BIT),
    [1] = ACTION_FUNCTION_OPT(PENTI_KEY, PENTI_INDEX_BIT),
    [2] = ACTION_FUNCTION_OPT(PENTI_KEY, PENTI_MIDDLE_BIT),
    [3] = ACTION_FUNCTION_OPT(PENTI_KEY, PENTI_RING_BIT),
    [4] = ACTION_FUNCTION_OPT(PENTI_KEY, PENTI_PINKY_BIT),
    [5] = ACTION_FUNCTION_OPT(PENTI_KEY, PENTI_REPEAT_BIT),

    [6] = ACTION_FUNCTION_TAP(LSHIFT_LPAREN),
    [7] = ACTION_FUNCTION_TAP(RSHIFT_RPAREN),
    [8] = ACTION_FUNCTION_TAP(AUTO_LBRACKET),
    [9] = ACTION_FUNCTION_TAP(AUTO_RBRACKET),
    [10] = ACTION_FUNCTION_TAP(AUTO_PAREN),
};

const macro_t *action_get_macro(keyrecord_t *record, uint8_t id, uint8_t opt) {
    return MACRO_NONE;
};


void matrix_init_user(void) {

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
        case PENTI_KEY:
            action_penti_key(record, opt);
            break;
        case LSHIFT_LPAREN:
            action_shift_paren(record, KC_LSHIFT);
            break;
        case RSHIFT_RPAREN:
            action_shift_paren(record, KC_RSHIFT);
            break;
        case AUTO_LBRACKET:
            action_shift_paren(record, KC_LBRACKET);
            break;
        case AUTO_RBRACKET:
            action_shift_paren(record, KC_RBRACKET);
            break;
        case AUTO_PAREN:
            action_auto_paren(record);
            break;
        default:
            break;
    }
}
