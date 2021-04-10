source ~/.shellrc

# If not running interactively, don't do anything.
# This snippet helps to fix scp, sftp "Received message too long" issue..
# [ -z "$PS1" ] && return

## Leftside prompt
# PROMPT=$'%F{blue}%F{CYAN}%B%F{cyan}%n %F{white}@ %F{magenta}%m %F{white}-> %F{green}%~ %1(j,%F{red}:%j,)%b:%F{blue}%F{CYAN}%B%F{cyan}$(git_prompt) \n%F{blue}%B%(?..[%?] )%{%F{red}%}%# %F{white}%b'
 PROMPT=$'%F{magenta}[%n] %F{green}>>> %B%F{cyan}%~ %1(j,%F{red}:%j,)%F{blue}$() \n%F{white}%B%(?..[%?] )%{%F{red}%}%# %F{white}%b'

# direct
function git_prompt() {
  (git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD ) 2> /dev/null | cut -d'/' -f3
}

## System plugins

# auto-highlight
if [[ -s /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]];then
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# Use history substring search
source ~/.zsh/zsh-history-substring-search.zsh
# bind UP and DOWN arrow keys to history substring search

zmodload zsh/terminfo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

if [[ -s ~/.zsh/zsh-autosuggestions.zsh ]]; then
  source ~/.zsh/zsh-autosuggestions.zsh
  bindkey '^F' autosuggest-accept
  ZSH_AUTOSUGGEST_USE_ASYNC=1
  ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
  ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=yellow,bg=black,bold,underline"
  ZSH_AUTOSUGGEST_STRATEGY=(history completion) # prompt history first
fi

#source function.sh if it exists
[ -f "$HOME/.local/etc/function.sh"  ] && . "$HOME/.local/etc/function.sh"

# bindkey
bindkey -v
bindkey '^p' history-incremental-search-forward
bindkey '^r' history-incremental-search-backward

bindkey '^e' end-of-line
bindkey '^a' beginning-of-line
bindkey '^f' forward-char #
bindkey '^b' backward-char #
bindkey '^d' delete-char #
bindkey '^p' history-substring-search-up
bindkey '^n' history-substring-search-down

# Navigate words with ctrl+arrow keys
bindkey '^[Oc' forward-word                                     #
bindkey '^[Od' backward-word                                    #
bindkey '^[[1;5D' backward-word                                 #
bindkey '^[[1;5C' forward-word                                  #
bindkey '^H' backward-kill-word                                 # delete previous word with ctrl+backspace
bindkey '^z' undo                                             #

#Required by autosuggestions
zmodload zsh/zpty

## zshstyle path
#typeset -U path
#path=(~/bin ~/.local/bin ~/.cargo/bin ~/.yarn/bin "$path[@]")
#path=( ${(u)^path:A}(N-/) )

# History {{{2
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history
unsetopt flowcontrol
setopt hist_ignore_all_dups     # when runing a command several times, only store one
setopt hist_reduce_blanks       # reduce whitespace in history
setopt hist_ignore_space        # do not remember commands starting with space
setopt histfcntllock            # use F_SETLCKW
setopt share_history            # share history among sessions
setopt extended_history         # timestamp for each history entry
setopt hist_verify              # reload full command when runing from history
setopt hist_expire_dups_first   # remove dups when max size reached
setopt inc_append_history       # append to history once executed
setopt notify                   # report the status of backgrounds jobs immediately

for i ({1..9}) alias $i="cd +$i"; unset i

# #--------------------------------------------------

setopt AUTO_LIST
setopt AUTO_MENU
setopt MENU_COMPLETE
setopt complete_in_word   # complete /v/c/a/p
setopt no_nomatch                 # enhanced bash wildcard completion
setopt magic_equal_subst
setopt noautoremoveslash
setopt null_glob
setopt no_complete_aliases
setopt auto_continue            #automatically send SIGCON to disowned jobs
setopt extended_glob            # so that patterns like ^() *~() ()# can be used
setopt pushd_ignore_dups        # do not push dups on stack
setopt brace_ccl                # expand alphabetic brace expressions
setopt correct                  # spell check for commands only
setopt equals
setopt no_hist_beep             # don not beep on history expansion errors
setopt interactive_comments     # comments in history
setopt list_types               # show ls -F style marks in file completion
setopt long_list_jobs           # show pid in bg job list
setopt numeric_glob_sort        # when globbing numbered files, use real counting
setopt prompt_subst             # prompt more dynamic, allow function in prompt
setopt nobeep
#setopt hash_list_all            # search all paths before command completion

zstyle ':completion:*' use-cache true
zstyle ':completion:*:*:default' force-list always
zstyle ':completion:*' verbose yes
zstyle ':completion:*' menu select

#path Completion
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-shlashes 'yes'
zstyle ':completion::complete:*' '\\'

#ignore the current directory
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# huge list
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*:default' menu 'select=0'

# Completing order
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' completer _complete _prefix _user_expand _correct _prefix _match

# Separate man page sections.
zstyle ':completion:*:manuals' separate-sections true

## Fix case and typo
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Grouping Completion
# 补全时分组给出结果
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:descriptions' format $'\e[01;33m -- %d --\e[0m'

#Application complete
zstyle ':completion:*' file-patterns '%p(^-/):globbed-files' '*(-/):directories' '*:all-files'
zstyle ':completion::complete:et:*' file-patterns '*.{xls,xlsx}:files:files *(/):directories'
zstyle ':completion::complete:evince:*' file-patterns '*.{pdf,ps,eps,dvi,djvu,pdf.gz,ps.gz,dvi.gz}:documents:documents *(-/):directories:directories'
zstyle ':completion::complete:feh:*' file-patterns '*.{gif,png,jpeg,jpg,svg}:images:images *(-/):directories:directories'
zstyle ':completion::complete:geeqie:*' file-patterns '*.{gif,png,jpeg,jpg,svg}:images:images *(-/):directories:directories'
zstyle ':completion::complete:llpp:*' file-patterns '*.pdf:files:files *(-/):directories:directories'
zstyle ':completion::complete:wpp:*' file-patterns '*.{ppt,pptx}:files:files *(/):directories'
zstyle ':completion::complete:wps:*' file-patterns '*.{doc,docx}:files:files *(/):directories'
zstyle ':completion::complete:display:*' file-patterns '*.{gif,png,jpeg,jpg,svg,webp}:images *(-/):directories'
zstyle ':completion::complete:x:*' file-patterns '*.{7z,bz2,gz,rar,tar,tbz,tgz,zip,chm,xz,exe,xpi,apk,maff,crx}:compressed-files:compressed\ files *(-/):directories:directories'
zstyle ':completion:*:*:vim:*:*files' ignored-patterns '*.(out|o|avi|mkv|rmvb|pyc|wmv|exe)'
zstyle ':completion::*:(-command-|export):*' fake-parameters CFLAGS CXXFLAGS LD_LIBRARY_PATH

zstyle ':completion::complete:e:*' file-patterns '*.*:files:files *(-/):directories:directories'
#kill completion
compdef pkill=kill
compdef pkill=killall
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:*:*:processes' force-list always
zstyle ':completion:*:processes' command 'ps -au$USER '
zstyle ':completion:*:processes-names' command  'ps c -u ${USER} -o command | uniq'

## Alias section
alias ls='ls --color=auto'
alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'

# Directories {{{2
setopt auto_cd                  # if not a command, try to cd to it.
setopt auto_pushd               # automatically pushd directories on dirstack

# Add notification to VI-mode
function zle-line-init zle-keymap-select {
VIMODE="${${KEYMAP/vicmd/|N|}/(main|viins)/|I|}"
zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select
VIMODE='|I|'
# RPROMPT='${VIMODE}%{$reset_color%}'

# Async directory in zshell
# https://emacs-china.org/t/emacs-builtin-mode/11937/83
# INSIDE_EMACS 则是 Emacs 在创建 term/shell/eshell 时都会带上的环境变量
# 通常 shell/tramp 会将 TERM 环境变量设置成
# dumb，所以这里要将他们排除。
function precmd() {
  if [[ -n "$INSIDE_EMACS" && "$TERM" != "dumb" ]]; then
    echo -e "\033AnSiTc" "$(pwd)"
    # echo -e "\033AnSiTh" $(hostname -f)
    echo -e "\033AnSiTu" "$LOGNAME"
  fi
}
