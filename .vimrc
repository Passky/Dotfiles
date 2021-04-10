"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:coc_nvim_disablekeymap = 0
set cpt=.,k,w,t,b,t,i
source ~/.vim/setup/keymap.vim
if filereadable(expand($HOME . '/.vimrc.mine'))
    source ~/.vimrc.mine
endif
"source ~/.vim/setup/autopop.vim
"source $HOME/.vim/setup/root.vim

call plug#begin('~/.vim/plugged')
" Plug 'airblade/vim-rooter'
" Plug 'rhysd/8cc.vim'
Plug 'mhinz/vim-startify'
Plug 'Yggdroot/indentLine'
Plug 'easymotion/vim-easymotion'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!'] }
Plug 'jiangmiao/auto-pairs'
Plug 'morhetz/gruvbox'
nnoremap <silent> <space> :WhichKey '<Space>'<CR>
nmap <silent> g :WhichKey 'g'<CR>
nmap gg :1<cr>
nmap gj gj
nmap gk gk

Plug 'skywind3000/asyncrun.vim'
source ~/.vim/setup/asyncrun.vim

if executable('node')
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    source ~/.vim/setup/coc-nvim.vim
else
    let g:coc_nvim_disablekeymap = 1
endif

if  executable('ccls') || executable('clangd')
    Plug 'jackguo380/vim-lsp-cxx-highlight'
else
    Plug 'octol/vim-cpp-enhanced-highlight'
    let g:cpp_concepts_highlight = 1
    let g:cpp_class_scope_highlight = 1
    let g:cpp_member_variable_highlight = 1
    let g:cpp_class_decl_highlight = 1
    let g:cpp_posix_standard = 1
    let g:cpp_experimental_template_highlight = 1
    let c_no_curly_error=1
endif

if  executable('ctags') || executable('global')
    if executable("pygmentize")
        let $GTAGSLABEL = 'native-pygments' " let global use pygments as frontend
    else
        let $GTAGSLABEL = 'native' " let global use native frontend
    endif
    Plug 'ludovicchabant/vim-gutentags'
    Plug 'skywind3000/gutentags_plus'
    source ~/.vim/setup/vim-gutentag.vim
endif

if has('python3') || has('python')
    Plug 'Yggdroot/LeaderF'
    source ~/.vim/setup/leaderf.vim
endif

call plug#end()

" map for heavy wrapline work
" noremap j gj
" noremap k gk
inoremap kj <esc>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" configration for Basic
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader = "\\" " set \ as the Leader
syntax enable            " syntax highlight on
set lazyredraw           " lazyredraw for perfomance
set cmdheight=1          " set the height of cmdline
set noshowmode           " don't show mode on cmdline
set showcmd              " show selected lines for mode(v)
set ruler                " always show cursor place
set mouse=a              " enable mouse
set laststatus=2         " always show statuline
set number               "
set cursorline           " highlight current line
set whichwrap+=<,>,h,l   " set cursor cross line
set ttimeoutlen=0        " respond time for \esc
set virtualedit=block,onemore   " allow cursor behind last letter
set belloff=all
set nocompatible         " no compatible with legacy vi
" expand to filetype on(give a judge for filetype)/filetype plugin on(enable plugin by filetype)/filetype indent on(indent by filetype)
filetype plugin indent on
" back to the last edit cursor
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | execute "normal! g'\"" | endif
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 代码缩进和排版
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o "取消粘贴时自动注释
se so=3                  " left 4 line while scrolling up or down
set autoindent           " set autoindent
set smartindent          " smarter indent
set cindent              " indent method for c/c++
set softtabstop=4        " set a tab to 4 \space
set tabstop=4            " ...while edit
set shiftwidth=4         " ...while format
set expandtab            " expand tab to \space
set smarttab             " auto tab at start of paragraph or line
set backspace=2          " treat indent,eol,start normally with \cr
set sidescroll=10        " left 10 column while scrolling left or right
set nofoldenable         " no fold
set splitright           " open new split window on right
set wrap                 " open wrap
set linebreak            " smart wrap
syntax sync minlines=300 " limit of highlight line once
set synmaxcol=300 " limit of highlight column once
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 代码补全
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>" "enter to confirm
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<tab>"  " tab to cycle
let g:termdebug_wide=163  "默认竖屏
set omnifunc=syntaxcomplete#Complete " Omni completion
set tags=./.tags;,.tags
set wildmenu             " open completion for Vim's command line
set completeopt=menu,menuone,noinsert
set completeopt-=preview " Not to open a new preview window
" Don't pass messages to |ins-completion-menu|.
set shortmess+=c
set shortmess-=S        " 显示搜索计数器"
autocmd CompleteDone * if !pumvisible() | pclose | endif
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"搜索设置
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nohlsearch          " not to highlight search result
set incsearch           " in time search preview
set ignorecase          " ignore case
set smartcase           " smart care
set showmatch           " match parentheses
set matchtime=2
" Preview before replace
if has("nvim")
    if exists('+inccommand')
        set inccommand=nosplit
    endif
endif
" Command line & vimgrep filter
if has('wildmenu')
    set wildignorecase
    set wildignore+=.git,.hg,.svn,.stversions,*.pyc,*.spl,*.o,*.out,*~,%*,*.exe,tags
    set wildignore+=.ccls-cache,.clangd,compile_commands.json,*.ninja,*.cmake,*.bin,CMakeC*,CMakeO*
    set wildignore+=*.jpg,*.jpeg,*.png,*.gif,*.zip,**/tmp/**,*.DS_Store
    set wildignore+=**/node_modules/**,**/bower_modules/**,*/.sass-cache/*
    set wildignore+=application/vendor/**,**/vendor/ckeditor/**,media/vendor/**
    set wildignore+=__pycache__,*.egg-info,.pytest_cache,.mypy_cache/**
endif
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" configration for Buffer
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nobackup            " set not to backup
set noswapfile          " set no swap file
set autoread            " autoread while the file change
set autowrite           " autosave
set confirm             " confirm before leave unsaved buffer
set undofile
set undodir=/tmp
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" configration for Encoding
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set termencoding=utf-8
set fileformat=unix
set encoding=utf8
set fileencodings=utf8,ucs-bom,gbk,cp936,gb2312,gb18030
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" configration for Theme
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
colorscheme gruvbox
" if strftime("%H") < 18 && strftime("%H") > 6
"   set background=light
" else
set background=dark
" endif
if has("termguicolors")
    if !has("nvim")
        let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
        let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    endif
    set termguicolors
else
    set t_Co=256             " support for 256bit color
    " color for completion label
    hi Pmenu ctermfg=gray ctermbg=black guibg=#444444
    hi PmenuSel ctermfg=7 ctermbg=4 guibg=#555555 guifg=#ffffff
endif
packadd termdebug "Prevent a highlight bug

" configration for Matchit
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" packadd! matchit     "works in later version of vim
runtime macros/matchit.vim
let b:match_word='\<begin\>:\<end\>,'
            \ . '\<while\>:\<continue\>:<break\>:\<endwhile\>,'
            \ . '\<if\>:\<else if\>:<else\>,'
            \ . '\<module\>:\<endmodule\>,'
            \ . '\<task\>:\<endtask\>,'
            \ . '\<function\>:\<endfunction\>,'
            \ . '\<program\>:\<endprogram\>'
            \ . '\<func\>:\<endfunc\>,'
let b:matchit_ignorecase=1    " ignore case
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" configration for Statuline
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! Buf_total_num()
    return len(filter(range(1, bufnr('$')), 'buflisted(v:val)'))
endfunction
function! File_size(f)
    let l:size = getfsize(expand(a:f))
    if l:size == 0 || l:size == -1 || l:size == -2
        return ''
    endif
    if l:size < 1024
        return l:size.' bytes'
    elseif l:size < 1024*1024
        return printf('%.1f', l:size/1024.0).'k'
    elseif l:size < 1024*1024*1024
        return printf('%.1f', l:size/1024.0/1024.0) . 'm'
    else
        return printf('%.1f', l:size/1024.0/1024.0/1024.0) . 'g'
    endif
endfunction

" if buffer is modified,return 🏃
function! Tell_modified_or_nor()
    let i = bufnr('%')  " get num of current buffer
    if getbufinfo(i)[0].changed
        return "🏃" " run faster !
    else
        return "😭" " Oh,my poor man,you just can't write any more right?
    endif
endfunction

set statusline=                                 " clear status line
set statusline+=%*\ %{Tell_modified_or_nor()}
set statusline+=%1*\ %<%.50F\ %*
set statusline+=%=%*\ %{&ff}\|%{\"\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")&&&bomb)?\",B\":\"\").\"\|\"}%-9.(%l:%c%V%)%*
set statusline+=%*%{File_size(@%)}%*
set statusline+=%*\ %y%h%m%r%w\ %*
set statusline+=%*%3p%%\ \%*            " Texts before cursor/Total
set statusline+=%*\ %{mode()}\ 

" hi User1 cterm=none ctermfg=green ctermbg=0
" hi StatusLine term=reverse ctermbg=red gui=undercurl guisp=Magenta
" if version >= 700
"     au InsertEnter * hi StatusLine term=reverse ctermbg=grey gui=undercurl guisp=Magenta
"     au InsertLeave * hi StatusLine term=reverse ctermbg=red gui=undercurl guisp=Magenta
" endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" configration for Gui
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("gui_running")
    let system = system('uname -s')
    if system == "Darwin\n"
        "autocmd GUIEnter * simalt ~x
    elseif system == "Linux\n"
        "autocmd GUIEnter * simalt ~x
    else
        set renderoptions=type:directx,renmode:5,taamode:1
        set guifont=Consolas:h12:cANSI
        "set guifont=Courier_New:h14:cANSI
    endif
    "set guioptions-=m           " hide menu bar
    "set showtabline=0           " hide tab bar
    set guioptions-=T           " hide tool-bar
    set guioptions-=L           " hide left scroll
    set guioptions-=r           " hide right scroll
    set guioptions-=b           " hide buttom scroll
    set guicursor=n-v-c:ver5    " set cursor to narrow line
endif
