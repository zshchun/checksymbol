# CheckSymbol
Vim plugin for linux kernel study

# Installation
Put a file `checksymbol.vim` into `$HOME/.vim/plugin`

# Keyboard shortcut
`<C-c>` : Display hexadecimal and binary values or Check current kernel configuration (.config)

`\`   : C-style calculator

        ex) 0x1234 & ((1 << 12) -1)

##  GIT features
`<C-g>` : blame (support visual block)

`gl`    : logs (support visual block)

`<CR>`  : select log, commit

`d`     : show diff

`D`     : show all changeset

`]`     : find next

`[`     : find previous

`q`     : close window

# Options
let g:git_window = [vert, hori(default), none]

let g:git_resize = [vert, hori, both(default), none]

let g:git_scroll = [top, center(default), none]

let g:git_merges = [0, 1(default)]
