"-------------------------------------------
"CoC.nvim Family
"关闭低版本vim时coc的提示
if filereadable(expand($HOME . '/.vim/pluginkeymap.vim'))
    source $HOME/.vim/pluginkeymap.vim
endif
let g:coc_disable_startup_warning = 1 

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
    " Recently vim can merge signcolumn and number column into one
    set signcolumn=number
else
    set signcolumn=yes
endif

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

"---------这里主要是实现在 Ax 的位置下,x处按tab可以触发补全
" " Use tab for trigger completion with characters ahead and navigate.
" " NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" " other plugin before putting this into your config.
 " inoremap <silent><expr> <TAB>
 "             \ pumvisible() ? "\<C-n>" :
 "             \ <SID>check_back_space() ? "\<TAB>" :
 "             \ coc#refresh()
 " inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
 " function! s:check_back_space() abort
 "     let col = col('.') - 1
 "     return !col || getline('.')[col - 1]  =~# '\s'
 " endfunction

" Coc-pairs
autocmd FileType c let b:coc_pairs_disabled = ['<']
autocmd FileType cpp let b:coc_pairs_disabled = ['<']

nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    else
        call CocAction('doHover')
    endif
endfunction

if get(g:, 'coc_nvim_disablekeymap', 0) == 0
    " GoTo definition
    "nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gy <Plug>(coc-type-definition)
    nmap <silent> gr <Plug>(coc-references)
endif

" Use `_[` and `_]` to navigate diagnostics
nmap <silent> <space>[ <Plug>(coc-diagnostic-prev)
nmap <silent> <space>] <Plug>(coc-diagnostic-next)
nmap <space>cr <Plug>(coc-rename)
nmap <space>cR <Plug>(coc-refactor)
nmap <space>cf  <Plug>(coc-fix-current)
nnoremap <space>ci :CocCommand clangd.symbolInfo<cr>
nnoremap <space>ch :CocCommand clangd.switchSourceHeader<cr>

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>cld  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>cle  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>clc  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>clf  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>clt  :<C-u>CocList -I symbols<cr>
" Resume latest coc list.
nnoremap <silent><nowait> <space>clr  :<C-u>CocListResume<CR>
" most recent use
nnoremap <silent><nowait> <space>clm  :<C-u>CocList mru<CR>
"files 
nnoremap <silent><nowait> <space>cls  :<C-u>CocList files<CR>
"
nnoremap <silent><nowait> <space>clk  :<C-u>CocList maps<CR>

nnoremap <silent><nowait> <space>clb  :<C-u>CocList buffers<CR>
nnoremap <silent><nowait> <space>cla  :<C-u>CocList tags<CR>
nnoremap <silent><nowait> <space>cll  :<C-u>CocList lines<CR>
nnoremap <silent><nowait> <space>clg  :<C-u>CocList grep<CR>
nnoremap <silent><nowait> <space>clw  :<C-u>CocList words<CR>

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')
nnoremap <silent> <space>cm  :Format<cr>

"coc-git
nnoremap <space>gc :CocCommand git.ShowCommit<cr>

"------------------CClS language server------------
nnoremap <silent> <space>xb :call CocLocations('ccls','$ccls/inheritance')<cr>
" bases of up to 3 levels
nnoremap <silent> <space>xB :call CocLocations('ccls','$ccls/inheritance',{'levels':3})<cr>
" derived
nnoremap <silent> <space>xd :call CocLocations('ccls','$ccls/inheritance',{'derived':v:true})<cr>
" derived of up to 3 levels
nnoremap <silent> <space>xD :call CocLocations('ccls','$ccls/inheritance',{'derived':v:true,'levels':3})<cr>
" caller
nnoremap <silent> <space>xc :call CocLocations('ccls','$ccls/call')<cr>
" callee
nnoremap <silent> <space>xC :call CocLocations('ccls','$ccls/call',{'callee':v:true})<cr>
" $ccls/member
" member variables / variables in a namespace
nnoremap <silent> <space>xm :call CocLocations('ccls','$ccls/member')<cr>
" member functions / functions in a namespace
nnoremap <silent> <space>xf :call CocLocations('ccls','$ccls/member',{'kind':3})<cr>
" nested classes / types in a namespace
nnoremap <silent> <space>xs :call CocLocations('ccls','$ccls/member',{'kind':2})<cr>

nnoremap <silent> <space>xt <Plug>(coc-type-definition)<cr>
nnoremap <silent> <space>xv :call CocLocations('ccls','$ccls/vars')<cr>
nnoremap <silent> <space>xV :call CocLocations('ccls','$ccls/vars',{'kind':1})<cr>

