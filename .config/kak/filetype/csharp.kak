#
# Basically a modified version of <kak>/rc/filetype/java.kak.
# Replaced all 'java' with 'csharp', and added some C# keywords.
#
# The original code was licensed under the Unlicense.
#

hook global BufCreate .*\.(cs|csx)$ %{
    set-option buffer filetype csharp
}

# Initialization
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾

hook global WinSetOption filetype=csharp %{
    require-module csharp

    # cleanup trailing whitespaces when exiting insert mode
    hook window ModeChange pop:insert:.* -group csharp-trim-indent %{ try %{ execute-keys -draft <a-x>s^\h+$<ret>d } }
    hook window InsertChar \n -group csharp-insert csharp-insert-on-new-line
    hook window InsertChar \n -group csharp-indent csharp-indent-on-new-line
    hook window InsertChar \{ -group csharp-indent csharp-indent-on-opening-curly-brace
    hook window InsertChar \} -group csharp-indent csharp-indent-on-closing-curly-brace

    hook -once -always window WinSetOption filetype=.* %{ remove-hooks window csharp-.+ }
}

hook -group csharp-highlight global WinSetOption filetype=csharp %{
    add-highlighter window/csharp ref csharp
    hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/csharp }
}

provide-module csharp %§

add-highlighter shared/csharp regions
add-highlighter shared/csharp/code default-region group
add-highlighter shared/csharp/string region %{(?<!')"} %{(?<!\\)(\\\\)*"} fill string
add-highlighter shared/csharp/character region %{'} %{(?<!\\)'} fill value
add-highlighter shared/csharp/comment region /\* \*/ fill comment
add-highlighter shared/csharp/inline_documentation region /// $ fill documentation
add-highlighter shared/csharp/line_comment region // $ fill comment

add-highlighter shared/csharp/code/ regex %{\b(this|true|false|null)\b} 0:value
add-highlighter shared/csharp/code/ regex "\b(void|byte|sbyte|short|ushort|int|uint|long|ulong|char|float|bool|double|string)\b" 0:type
add-highlighter shared/csharp/code/ regex "\b(while|for|if|else|do|static|switch|case|default|class|interface|enum|goto|break|continue|return|try|catch|throw|new|as|base|checked|const|decimal|delegate|event|explicit|extern|finally|fixed|foreach|implicit|in|internal|is|lock|namespace|object|operator|out|override|params|readonly|ref|sealed|sizeof|stackalloc|struct|typeof|unchecked|unsafe|using|virtual)\b" 0:keyword
add-highlighter shared/csharp/code/ regex "\b(final|public|protected|private|abstract|synchronized|native|transient|volatile)\b" 0:attribute
add-highlighter shared/csharp/code/ regex "(?<!\w)@\w+\b" 0:meta

# Commands
# ‾‾‾‾‾‾‾‾

define-command -hidden csharp-insert-on-new-line %[
        # copy // comments prefix and following white spaces
        try %{ execute-keys -draft <semicolon><c-s>k<a-x> s ^\h*\K/{2,}\h* <ret> y<c-o>P<esc> }
]

define-command -hidden csharp-indent-on-new-line %~
    evaluate-commands -draft -itersel %=
        # preserve previous line indent
        try %{ execute-keys -draft <semicolon>K<a-&> }
        # indent after lines ending with { or (
        try %[ execute-keys -draft k<a-x> <a-k> [{(]\h*$ <ret> j<a-gt> ]
        # cleanup trailing white spaces on the previous line
        try %{ execute-keys -draft k<a-x> s \h+$ <ret>d }
        # align to opening paren of previous line
        try %{ execute-keys -draft [( <a-k> \A\([^\n]+\n[^\n]*\n?\z <ret> s \A\(\h*.|.\z <ret> '<a-;>' & }
        # indent after a switch's case/default statements
        try %[ execute-keys -draft k<a-x> <a-k> ^\h*(case|default).*:$ <ret> j<a-gt> ]
        # indent after keywords
        try %[ execute-keys -draft <semicolon><a-F>)MB <a-k> \A(if|else|while|for|try|catch)\h*\(.*\)\h*\n\h*\n?\z <ret> s \A|.\z <ret> 1<a-&>1<a-space><a-gt> ]
        # deindent closing brace(s) when after cursor
        try %[ execute-keys -draft <a-x> <a-k> ^\h*[})] <ret> gh / [})] <ret> m <a-S> 1<a-&> ]
    =
~

define-command -hidden csharp-indent-on-opening-curly-brace %[
    # align indent with opening paren when { is entered on a new line after the closing paren
    try %[ execute-keys -draft -itersel h<a-F>)M <a-k> \A\(.*\)\h*\n\h*\{\z <ret> s \A|.\z <ret> 1<a-&> ]
]

define-command -hidden csharp-indent-on-closing-curly-brace %[
    # align to opening curly brace when alone on a line
    try %[ execute-keys -itersel -draft <a-h><a-k>^\h+\}$<ret>hms\A|.\z<ret>1<a-&> ]
]

§
