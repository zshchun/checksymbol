# CheckSymbol
Vim plugin for linux kernel study

# Installation
Put a file `checksymbol.vim` into `$HOME/.vim/plugin`

# Keyboard shortcut
`<CR>` : Display hexadecimal and binary values

         Check current kernel configuration (.config)

         Jump to the definition of the keyword (tag)

`\`   : C-style calculator

        ex) 0x1234 & ((1 << 12) -1)

##  GIT features
`gb` : blame (support visual block)

`gl`    : logs (support visual block)

`<CR>`  : select log, show diff

`d`     : show diff

`s`     : show all changeset

`[`     : find previous

`]`     : find next

`q`     : close window

# Options
let g:git_window = [vert, hori(default), none]

let g:git_resize = [vert, hori, both(default), none]

let g:git_scroll = [top, center(default), none]

let g:git_merges = [0, 1(default)]
