" LeaderF
let g:Lf_WildIgnore = {
            \ 'dir': ['.svn','.git','.hg','.vscode','.wine','.deepinwine','.oh-my-zsh'],
            \ 'file': ['*.sw?','~$*','*.bak','*.exe','*.o','*.so','*.py[co]']
            \}
let g:Lf_RootMarkers = ['.project', '.root', '.svn', '.git']
let g:Lf_WorkingDirectoryMode = 'Ac'
let g:Lf_UseCache = 1
let g:Lf_CacheDirectory = expand($HOME . '/.cache')
let g:Lf_WindowHeight = 0.30
let g:Lf_ShortcutF = "<leader>fs"
let g:Lf_ShortcutB = "<leader>fb"
" let g:Lf_WindowPosition = 'popup'
" let g:Lf_PreviewInPopup = 1
" set ambiwidth=doubl://www.zhihu.com/question/47691414/answer/373700711

" noremap gi :Leaderf bufTag --fuzzy --popup<cr>
" rg should take references of ripgrep instead of Leaderfc
" nnoremap <space>fg  :Leaderf rg<space>
noremap <space>fb :Leaderf buffer<cr>
" find-file
nnoremap <space>ff :LeaderfFile <cr>
" Symbols of workspace
nnoremap <space>fs :Leaderf! function<cr> 
nnoremap <space>ft  :Leaderf! bufTag<cr>
nnoremap <space>fg  :CocList grep<cr>
nnoremap <space>fa  :Leaderf! tag<cr>
nnoremap <space>fl  :Leaderf line --popup<cr>
nnoremap <space>fm  :Leaderf mru --popup<cr>
nnoremap <space>fc  :Leaderf command --popup<cr>
nnoremap gi :Leaderf! bufTag<cr>
