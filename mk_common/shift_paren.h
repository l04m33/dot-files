#ifndef SHIFT_PAREN_H_INCLUDED
#define SHIFT_PAREN_H_INCLUDED

typedef struct {
    uint8_t enabled;
} auto_paren_state_t;

static auto_paren_state_t auto_paren_state = {
    .enabled = 0,
};

static void action_shift_paren(keyrecord_t *record, uint8_t shift_kc)
{
    if (record->event.pressed) {
        if (record->tap.count <= 0 || record->tap.interrupted) {
            register_mods(MOD_BIT(shift_kc));
        }
    } else {
        if (record->tap.count > 0 && !record->tap.interrupted) {
            /* To minimize interference with other modifiers,
             * only do the auto-pairing when no modifier is added.
             */
            if (auto_paren_state.enabled && !(get_mods() | get_weak_mods())) {
                switch (shift_kc) {
                    case KC_LSHIFT:
                        action_macro_play(
                            MACRO(
                                I(10),
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

#ifndef KEY_TAPPED
#define KEY_TAPPED(_rec_, _count_) ((_rec_)->tap.count == (_count_) && !(_rec_)->tap.interrupted)
#endif

static void action_auto_paren(keyrecord_t *record)
{
    if (!record->event.pressed && KEY_TAPPED(record, 1)) {
        auto_paren_state.enabled = 1 - auto_paren_state.enabled;
        dprintf("auto paren: %d\n", auto_paren_state.enabled);
    }
}

#endif // SHIFT_PAREN_H_INCLUDED
