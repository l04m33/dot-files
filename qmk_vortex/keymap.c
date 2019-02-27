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

#include "vortex.h"

enum keycodes {
    DUMMY = SAFE_RANGE,
    DYNAMIC_MACRO_RANGE,
};

#include "dynamic_macro.h"

/************************************************
 * keymaps
 ************************************************/

const uint16_t keymaps_default[][MATRIX_ROWS][MATRIX_COLS] = {
#if defined(KEYMAP_VORTEX_CORE)
    /* default layer */
    [0] = LAYOUT_core(
        KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,         KC_T,         KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC, KC_RBRC,
        KC_LCTL, KC_A,    KC_S,    KC_D,    KC_F,         KC_G,         KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, MT(MOD_RCTL,KC_ENT),
        F(6),    KC_Z,    KC_X,    KC_C,    KC_V,         KC_B,         KC_N,    KC_M,    KC_COMM, KC_DOT,  F(7),    MO(2),
        KC_ESC,  KC_LGUI, KC_LALT, KC_BSPC, LT(3,KC_SPC), LT(3,KC_SPC), MO(1),   KC_RALT, KC_APP,  KC_RCTL
    ),
    /* Fn layer */
    [1] = LAYOUT_core(
        _______, _______, _______, _______, _______, _______, _______, KC_PGUP, KC_UP,   KC_PGDN, KC_PSCR, KC_SLCK, KC_PAUS,
        KC_CAPS, KC_MPRV, KC_MPLY, KC_MNXT, _______, _______, KC_HOME, KC_LEFT, KC_DOWN, KC_RIGHT,KC_INS,  KC_ENT,
        KC_LSFT, _______, _______, _______, _______, _______, KC_END,  _______, _______, _______, KC_RSFT, _______,
        _______, _______, _______, KC_DEL,  _______, _______, _______, _______, _______, _______
    ),
    /* Fn1 layer */
    [2] = LAYOUT_core(
        KC_GRV,          KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   KC_F6,          KC_F7,        KC_F8,   KC_F9,   KC_F10,  KC_F11, KC_F12,
        KC_1,            KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,           KC_8,         KC_9,    KC_0,    KC_MINS, KC_EQL,
        _______,         _______, _______, _______, _______, KC_QUOT, KC_SLSH,        KC_LBRC,      KC_RBRC, KC_BSLS, F(8),    _______,
        DYN_MACRO_PLAY1, _______, _______, _______, _______, _______, DYN_REC_START1, DYN_REC_STOP, _______, _______
    ),
    /* Space Fn layer */
    /* XXX: mouse keys are not working due to lack of endpoints (?) */
    [3] = LAYOUT_core(
        TG(4),   _______, KC_WH_L, KC_WH_U, KC_WH_D, KC_WH_R, KC_HOME, KC_PGDN, KC_PGUP, KC_END,  _______, _______, _______,
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

/************************************************
 * penti mode
 ************************************************/

/* This is chosen arbitrarily. Any unused key code will do. */
#define PENTI_KC_RESET KC_CLEAR

typedef struct {
    uint8_t key_code;
    uint8_t modifiers;
} penti_chord_map_entry_t;

static const penti_chord_map_entry_t penti_alpha_chord_map[] = {
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
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = KC_G },
    { .key_code = KC_Y },
    { .key_code = KC_V },
    { .key_code = KC_X },
    { .key_code = KC_M },
    { .key_code = KC_T },
    { .key_code = KC_K },
    { .key_code = KC_W },
};

static const penti_chord_map_entry_t penti_shift_chord_map[] = {
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
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = KC_G, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_Y, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_V, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_X, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_M, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_T, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_K, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_W, .modifiers = MOD_BIT(KC_LSHIFT) },
};

static const penti_chord_map_entry_t penti_punct_chord_map[] = {
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
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = KC_EQUAL },    // =
    { .key_code = KC_6,        .modifiers = MOD_BIT(KC_LSHIFT) }, // ^
    { .key_code = KC_9,        .modifiers = MOD_BIT(KC_LSHIFT) }, // (
    { .key_code = KC_BSLASH },   // '\'
    { .key_code = KC_DOT,      .modifiers = MOD_BIT(KC_LSHIFT) }, // >
    { .key_code = KC_5,        .modifiers = MOD_BIT(KC_LSHIFT) }, // %
    { .key_code = KC_2,        .modifiers = MOD_BIT(KC_LSHIFT) }, // @
    { .key_code = KC_COMMA,    .modifiers = MOD_BIT(KC_LSHIFT) }, // <
};

static const penti_chord_map_entry_t penti_digit_chord_map[] = {
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
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = KC_9 },
    { .key_code = KC_UP },
    { .key_code = KC_DOWN },
    { .key_code = KC_PGDOWN },
    { .key_code = KC_MINUS },
    { .key_code = KC_GRAVE, .modifiers = MOD_BIT(KC_LSHIFT) },
    { .key_code = KC_NO },     // <empty>
    { .key_code = KC_INSERT },
};

static const penti_chord_map_entry_t penti_funct_chord_map[] = {
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
    { .key_code = KC_COPY },
    { .key_code = KC_NO },     // QUIT
    { .key_code = KC_F8 },
    { .key_code = KC_PAUSE },  // BREAK
    { .key_code = KC_F10 },
    { .key_code = KC_PASTE },
    { .key_code = KC_NO },     // 0x10, NUM
    { .key_code = KC_NO },     // DEF
    { .key_code = KC_NO },     // <empty>
    { .key_code = KC_NO },     // HELP
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = PENTI_KC_RESET },
    { .key_code = KC_F9 },
    { .key_code = KC_NO },     // <empty>
    { .key_code = KC_NO },     // <empty>
    { .key_code = KC_NO },     // PASTE2
    { .key_code = KC_F12 },
    { .key_code = KC_NO },     // <empty>
    { .key_code = PENTI_KC_RESET },
    { .key_code = KC_NO },     // <empty>
};

typedef enum {
    PENTI_THUMB_BIT = 0,
    PENTI_INDEX_BIT,
    PENTI_MIDDLE_BIT,
    PENTI_RING_BIT,
    PENTI_PINKY_BIT,
    PENTI_REPEAT_BIT,
} penti_bit_t;

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

/************************************************
 * penti mode end
 ************************************************/

/************************************************
 * space cadet shift keys & auto paren mode
 ************************************************/

typedef struct {
    uint8_t enabled;
} auto_paren_state_t;

static auto_paren_state_t auto_paren_state = {
    .enabled = 0,
};

/************************************************
 * space cadet shift keys & auto paren mode end
 ************************************************/

enum function_id {
    PENTI_KEY,
    LSHIFT_LPAREN,
    RSHIFT_RPAREN,
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
    [8] = ACTION_FUNCTION_TAP(AUTO_PAREN),
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

/************************************************
 * penti mode
 ************************************************/

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

static void penti_tap_hw_key(uint8_t key_code, uint8_t modifiers)
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

static void handle_penti_chord(uint8_t combo)
{
    penti_chord_map_stack_entry_t *stack_entry = get_chord_map();
    penti_chord_map_entry_t entry = stack_entry->map[combo];

    if (entry.key_code == PENTI_KC_RESET) {
        while (pop_chord_map());
        penti_state.extra_modifiers = 0;
        penti_state.extra_modifiers_transient = 0;
    } else if (entry.key_code != KC_NO) {
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
    }
}

static void arpeggio_switch_chord_map(uint8_t b0, uint8_t bn, uint8_t first_bit,
                                      const penti_chord_map_entry_t *map)
{
    if (first_bit == b0) {
        push_chord_map(map, 1);
    } else if (first_bit == bn) {
        if ((get_chord_map())->map == map) {
            pop_chord_map();
        } else {
            push_chord_map(map, 0);
        }
    } else {
        dprintf("  XXX: Inconsistent arpeggio bits");
    }
}

static void arpeggio_switch_modifier(uint8_t b0, uint8_t bn, uint8_t first_bit, uint8_t mod_bit)
{
    if (first_bit == b0) {
        penti_state.extra_modifiers |= mod_bit;
        penti_state.extra_modifiers_transient |= mod_bit;
    } else if (first_bit == bn) {
        penti_state.extra_modifiers_transient &= (~mod_bit);
        if (penti_state.extra_modifiers & mod_bit) {
            penti_state.extra_modifiers &= (~mod_bit);
        } else {
            penti_state.extra_modifiers |= mod_bit;
        }
    } else {
        dprintf("  XXX: Inconsistent arpeggio bits");
    }
}

static void arpeggio_tap_key(uint8_t b0, uint8_t bn, uint8_t first_bit,
                             uint8_t key_code_0, uint8_t key_code_n)
{
    if (first_bit == b0) {
        penti_tap_hw_key(key_code_0, penti_state.extra_modifiers);
        penti_state.to_repeat.key_code = key_code_0;
        penti_state.to_repeat.modifiers = penti_state.extra_modifiers;
        penti_clear_transient_modifiers();
    } else if (first_bit == bn) {
        penti_tap_hw_key(key_code_n, penti_state.extra_modifiers);
        penti_state.to_repeat.key_code = key_code_n;
        penti_state.to_repeat.modifiers = penti_state.extra_modifiers;
        penti_clear_transient_modifiers();
    } else {
        dprintf("  XXX: Inconsistent arpeggio bits");
    }
}

static void handle_penti_arpeggio(uint8_t combo, uint8_t ev_count, penti_event_t ev_list[])
{
    switch (combo) {

        case ((1 << PENTI_THUMB_BIT) | (1 << PENTI_INDEX_BIT)):
            arpeggio_switch_chord_map(PENTI_THUMB_BIT, PENTI_INDEX_BIT,
                                      ev_list[0].bit, penti_shift_chord_map);
            break;

        case ((1 << PENTI_THUMB_BIT) | (1 << PENTI_MIDDLE_BIT)):
            arpeggio_switch_chord_map(PENTI_THUMB_BIT, PENTI_MIDDLE_BIT,
                                      ev_list[0].bit, penti_punct_chord_map);
            break;

        case ((1 << PENTI_THUMB_BIT) | (1 << PENTI_RING_BIT)):
            arpeggio_switch_chord_map(PENTI_THUMB_BIT, PENTI_RING_BIT,
                                      ev_list[0].bit, penti_digit_chord_map);
            break;

        case ((1 << PENTI_MIDDLE_BIT) | (1 << PENTI_RING_BIT)):
            arpeggio_switch_chord_map(PENTI_MIDDLE_BIT, PENTI_RING_BIT,
                                      ev_list[0].bit, penti_funct_chord_map);
            break;

        case ((1 << PENTI_THUMB_BIT) | (1 << PENTI_PINKY_BIT)):
            arpeggio_switch_modifier(PENTI_THUMB_BIT, PENTI_PINKY_BIT,
                                     ev_list[0].bit, MOD_BIT(KC_LCTRL));
            break;

        case ((1 << PENTI_RING_BIT) | (1 << PENTI_PINKY_BIT)):
            arpeggio_switch_modifier(PENTI_RING_BIT, PENTI_PINKY_BIT,
                                     ev_list[0].bit, MOD_BIT(KC_LGUI));
            break;

        case ((1 << PENTI_INDEX_BIT) | (1 << PENTI_PINKY_BIT)):
            arpeggio_switch_modifier(PENTI_INDEX_BIT, PENTI_PINKY_BIT,
                                     ev_list[0].bit, MOD_BIT(KC_LALT));
            break;

        case ((1 << PENTI_INDEX_BIT) | (1 << PENTI_RING_BIT)):
            arpeggio_tap_key(PENTI_INDEX_BIT, PENTI_RING_BIT,
                             ev_list[0].bit, KC_ENTER, KC_ESCAPE);
            break;

        case ((1 << PENTI_INDEX_BIT) | (1 << PENTI_MIDDLE_BIT)):
            arpeggio_tap_key(PENTI_INDEX_BIT, PENTI_MIDDLE_BIT,
                             ev_list[0].bit, KC_TAB, KC_BSPACE);
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

/************************************************
 * penti mode end
 ************************************************/

/************************************************
 * space cadet shift keys & auto paren mode
 ************************************************/

static void action_shift_paren(keyrecord_t *record, uint8_t shift_kc)
{
    if (record->event.pressed) {
        if (record->tap.count <= 0 || record->tap.interrupted) {
            register_mods(MOD_BIT(shift_kc));
        }
    } else {
        if (record->tap.count > 0 && !record->tap.interrupted) {
            if (auto_paren_state.enabled) {
                switch (shift_kc) {
                    case KC_LSHIFT:
                        action_macro_play(
                            MACRO(
                                I(0),
                                D(LSHIFT),
                                T(9),
                                T(0),
                                U(LSHIFT),
                                T(LEFT),
                                END));
                        break;
                    case KC_RSHIFT:
                        action_macro_play(MACRO(T(RIGHT), END));
                        break;
                    default:
                        break;
                }
            } else {
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
            }
        } else {
            unregister_mods(MOD_BIT(shift_kc));
        }
    }
}

#define KEY_TAPPED(_rec_, _count_) ((_rec_)->tap.count == (_count_) && !(_rec_)->tap.interrupted)

static void action_auto_paren(keyrecord_t *record)
{
    if (!record->event.pressed && KEY_TAPPED(record, 1)) {
        auto_paren_state.enabled = 1 - auto_paren_state.enabled;
        dprintf("auto paren: %d\n", auto_paren_state.enabled);
    }
}

/************************************************
 * space cadet shift keys & auto paren mode end
 ************************************************/

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
        case AUTO_PAREN:
            action_auto_paren(record);
            break;
        default:
            break;
    }
}
