# Environment variables {{{1
set -x LESS '-MRi --shift 5'
set -x GREP_OPTIONS '--color=auto'
set -x MENUCONFIG_COLOR blackbg
set PATH ~/.dotnet ~/bin ~/.local/bin ~/.yarn/bin ~/.cargo/bin $PATH

function prepend_to_path -d "Prepend the given dir to PATH if it exists and is not already in it"
    if test -d $argv[1]
        if not contains $argv[1] $PATH
            set -gx PATH "$argv[1]" $PATH
        end
    end
end

# Greeting {{{2
set fish_greeting ''

# Bindings {{{1
fish_vi_key_bindings # use vim-mode
function fish_user_key_bindings
  bind \e. history-token-search-backward
end

# Aliases {{{1

# ls {{{2
alias ls 'command ls -XF --color=auto --time-style="+'\e'[33m['\e'[32m%Y-%m-%d '\e'[35m%k:%M'\e'[33m]'\e'[m"'
alias l 'ls -l'
alias la 'l -A'
alias lh 'l -h'
alias l1 'tree --dirsfirst -ChFL 1'
alias l2 'tree --dirsfirst -ChFL 2'
alias l3 'tree --dirsfirst -ChFL 3'
alias l4 'tree --dirsfirst -ChFL 4'

# coreutils {{{2
alias cp 'command cp -iv'
alias eg 'egrep -I'
alias mv 'command mv -iv'
alias rm 'command rm -ivd'

# others
alias psg 'ps aux | g'
alias 2pdf 'libreoffice --headless --convert-to pdf'
alias clip 'xsel -ib'
alias gdb 'command gdb -q'
alias port '/sbin/ss -ntlp'
alias rsync 'command rsync --progress --partial'
alias wgetpaste 'command wgetpaste -X'

# vim:sw=2 sts=2 et fdm=marker