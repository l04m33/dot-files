#ifndef SHIFT_PAREN_H_INCLUDED
#define SHIFT_PAREN_H_INCLUDED

typedef struct {
    uint8_t enabled;
} auto_paren_state_t;

static auto_paren_state_t auto_paren_state = {
    .enabled = 0,
};

static void action_shift_paren(keyrecord_t *record, uint8_t kc)
{
    if (record->event.pressed) {
        /* key down */
        if (record->tap.count <= 0 || record->tap.interrupted) {
            switch (kc) {
                case KC_LSHIFT:
                case KC_RSHIFT:
                    register_mods(MOD_BIT(kc));
                    break;
                case KC_LBRACKET:
                case KC_RBRACKET:
                    register_code(kc);
                    break;
                default:
                    break;
            }
        }
    } else {
        /* key up */
        if (record->tap.count > 0 && !record->tap.interrupted) {
            /* key tapped */
            if (auto_paren_state.enabled &&
                /* To minimize interference with other modifiers,
                 * only do the auto-pairing when no other modifier
                 * is added.
                 */
                !((get_mods() | get_weak_mods()) &
                  ~(MOD_BIT(KC_LSHIFT) | MOD_BIT(KC_RSHIFT)))) {
                /* auto paren enabled */
                switch (kc) {
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
                    case KC_LBRACKET:
                        action_macro_play(
                            MACRO(
                                I(10),
                                T(LBRACKET),
                                T(RBRACKET),
                                SM(),
                                CM(),
                                T(LEFT),
                                RM(),
                                END));
                        break;
                    case KC_RSHIFT:
                    case KC_RBRACKET:
                        action_macro_play(
                            MACRO(
                                SM(),
                                CM(),
                                T(RIGHT),
                                RM(),
                                END));
                        break;
                    default:
                        break;
                }
            } else {
                /* auto paren disabled */
                switch (kc) {
                    case KC_LSHIFT:
                    case KC_RSHIFT:
                        add_weak_mods(MOD_BIT(kc));
                        send_keyboard_report();
                        switch (kc) {
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
                        del_weak_mods(MOD_BIT(kc));
                        send_keyboard_report();
                        break;
                    case KC_LBRACKET:
                    case KC_RBRACKET:
                        register_code(kc);
                        unregister_code(kc);
                        break;
                    default:
                        break;
                }
            }
        } else {
            /* key pressed and then interrupted */
            switch (kc) {
                case KC_LSHIFT:
                case KC_RSHIFT:
                    unregister_mods(MOD_BIT(kc));
                    break;
                case KC_LBRACKET:
                case KC_RBRACKET:
                    unregister_code(kc);
                    break;
                default:
                    break;
            }
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
