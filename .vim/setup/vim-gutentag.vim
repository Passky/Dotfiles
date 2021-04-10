"-----------------------------------------------------------------------
"vim-gutentag
" gutentags 搜索工程目录的标志，碰到这些文件/目录名就停止向上一级目录递归
let g:gutentags_project_root = ['.root', '.svn', '.git', '.hg', '.project']
" 所生成的数据文件的名称
let g:gutentags_ctags_tagfile = '.tags'
" 同时开启 ctags 和 gtags 支持：
let g:gutentags_modules = []
if executable('ctags')
    let g:gutentags_modules += ['ctags']
endif
if executable('gtags-cscope') && executable('gtags')
    let g:gutentags_modules += ['gtags_cscope']
    let $GTAGSLABEL='native' "启用默认parser,支持java+c+cpp
    let g:gutentags_plus_nomap = 1
    if get(g:, 'coc_nvim_disablekeymap', 0) == 1
        nmap <silent> gr <Plug>GscopeFindSymbol  
        " reference
        nmap <silent> gd <Plug>GscopeFindDefinition 
        " definition
        nmap <silent> gy <Plug>GscopeFindCallingFunc 
        " Functions calling this function
    endif
    nmap <silent> gY <Plug>GscopeFindCalledFunc  
    " Functions called by this function
    nmap <silent> gt <Plug>GscopeFindText 
    " Find text string under cursor
    nmap <silent> ge <Plug>GscopeFindEgrep 
    " Find egrep pattern under cursor
    nmap <silent> gs <Plug>GscopeFindFile
    nmap <silent> gh <Plug>GscopeFindInclude
    nmap <silent> ga <Plug>GscopeFindAssign  
    " Find places where current symbol is assigned
    nmap <silent> gz <Plug>GscopeFindCtag
else
    nmap <silent> gr <Plug>GscopeFindCtag
endif
" 将自动生成的 tags 文件全部放入 ~/.cache/tags 目录中，避免污染工程目录
let s:vim_tags = expand('~/.cache/tags')
let g:gutentags_cache_dir = s:vim_tags
" 配置 ctags 的参数
"if has('win32') || has('win16') || has('win64') || has('win95')
let g:gutentags_ctags_extra_args = ['--output-format=e-ctags']  "新版本的输出和旧版本兼容
" endif
" let g:gutentags_ctags_extra_args += ['--fields=+niazS', '--extra=+q'] "老版本不能有--extra=+q
let g:gutentags_ctags_extra_args += ['--c++-kinds=+px']
let g:gutentags_ctags_extra_args += ['--c-kinds=+px']

" 禁用 gutentags 自动加载 gtags 数据库的行为
let g:gutentags_auto_add_gtags_cscope = 0

" 检测 ~/.cache/tags 不存在就新建
if !isdirectory(s:vim_tags)
    silent! call mkdir(s:vim_tags, 'p')
endif
" change focus to quickfix window after search (optional).
" let g:gutentags_plus_switch = 1
" nmap <silent> gk :GscopeKill<cr>
