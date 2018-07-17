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

#include "keyboard.h"
#include "action.h"
#include "unimap_trans.h"
#include "timer.h"
#if defined(__AVR__)
#   include <avr/pgmspace.h>
#endif


enum function_id {
    D_MACRO_RECORD,
    D_MACRO_PLAY,
};


#define AC_L1      ACTION_LAYER_TAP_TOGGLE(1)
#define AC_L2      ACTION_LAYER_TAP_TOGGLE(2)
#define AC_L3      ACTION_LAYER_TAP_KEY(3, KC_SPC)
#define AC_CTLENT  ACTION_MODS_TAP_KEY(MOD_RCTL, KC_ENT)

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
     *       |Alt|Gui  |          L3           | L2  |Alt|
     *       `-------------------------------------------'
     */
    [0] = UNIMAP_HHKB(
    ESC,  1,    2,    3,   4,   5,   6,   7,   8,    9,    0,    MINS, EQL,    BSLS, GRV,
    TAB,  Q,    W,    E,   R,   T,   Y,   U,   I,    O,    P,    LBRC, RBRC,   BSPC,
    LCTL, A,    S,    D,   F,   G,   H,   J,   K,    L,    SCLN, QUOT, CTLENT,
    LSFT, Z,    X,    C,   V,   B,   N,   M,   COMM, DOT,  SLSH, RSFT, L1,
          LALT, LGUI,           L3,                  L2,   RALT),

    /* layer 1: hhkb mode (hhkb fn) */
    [1] = UNIMAP_HHKB(
    PWR,  F1,   F2,   F3,   F4,   F5,   F6,   F7,   F8,   F9,   F10,  F11,  F12,  INS,  DEL,
    CAPS, TRNS, TRNS, TRNS, TRNS, TRNS, TRNS, TRNS, PSCR, SLCK, PAUS, UP,   TRNS, BSPC,
    TRNS, VOLD, VOLU, MUTE, TRNS, TRNS, PAST, PSLS, HOME, PGUP, LEFT, RGHT, PENT,
    TRNS, TRNS, TRNS, TRNS, TRNS, TRNS, PPLS, PMNS, END,  PGDN, DOWN, TRNS, L1,
          TRNS, TRNS,             SPC,                    TRNS, TRNS),

    /* layer 2: vi movement keys (right gui) */
    [2] = UNIMAP_HHKB(
    ESC,  F1,   F2,   F3,   F4,   F5,   F6,   F7,   F8,   F9,   F10,  F11,  F12,    INS,  DEL,
    TAB,  TRNS, UP,   TRNS, TRNS, TRNS, TRNS, PGUP, HOME, PGDN, TRNS, TRNS, TRNS,   BSPC,
    LCTL, LEFT, DOWN, RGHT, TRNS, TRNS, LEFT, DOWN, UP,   RGHT, TRNS, TRNS, CTLENT,
    LSFT, TRNS, TRNS, TRNS, TRNS, TRNS, END,  TRNS, TRNS, TRNS, TRNS, RSFT, TRNS,
          LALT, LGUI,             SPC,                    L2,   RALT),

    /* layer 3: mouse keys (space) */
    [3] = UNIMAP_HHKB(
    ESC,  F1,   F2,   F3,   F4,   F5,   F6,   F7,   F8,   F9,   F10,  F11,  F12,    INS,  DEL,
    TAB,  TRNS, TRNS, TRNS, TRNS, TRNS, WH_L, WH_D, WH_U, WH_R, TRNS, TRNS, TRNS,   BSPC,
    LCTL, TRNS, BTN3, BTN2, BTN1, TRNS, MS_L, MS_D, MS_U, MS_R, TRNS, TRNS, CTLENT,
    LSFT, ACL0, ACL1, ACL2, TRNS, TRNS, BTN1, BTN2, BTN3, TRNS, TRNS, RSFT, TRNS,
          LALT, LGUI,             L3,                     TRNS, RALT),
};
