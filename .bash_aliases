#!/bin/bash

alias find_non_ascii='grep --color='auto' -P -n "[\x80-\xFF]"'
alias ..="cd .."

# Rust cli replacements
alias ls="exa"
alias ll="exa -l"
alias la="exa -la"
alias cat="bat"
# alias less="bat"
# alias time="hyperfine"
alias rg="rg --no-heading"

# From agaggi
alias d2u="find . -type f -print0 | xargs -0 dos2unix"
# alias gitlog="git log --graph --pretty=format:'%Cred%h%Creset %C(bold blue)<%an>%Creset %<(1,trunc)%Cgreen(%cr) %<(1,trunc)%C(yellow)%d%Creset %s ' --abbrev-commit -n"
# alias make_build="/usr/bin/make"
# alias vim="nvim"
# alias code="/local/mnt/workspace/sw/vscode/bin"

function vimfd()
{
  vim ./$(fd $1)
}

# Emacs configurations
alias  e="emacs --with-profile legacy"
alias eb="emacs --with-profile evil-basic"
alias me="emacs --with-profile modern"
