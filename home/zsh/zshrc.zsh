# make / and . as delimiter for word (useful when deleting word backward)
autoload -U select-word-style
select-word-style bash

# zsh-syntax-highlighting
typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[alias]="fg=121"
ZSH_HIGHLIGHT_STYLES[builtin]="fg=121"
ZSH_HIGHLIGHT_STYLES[command]="fg=121"
ZSH_HIGHLIGHT_STYLES[precommand]="fg=051"

