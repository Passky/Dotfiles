nnoremap <space><space>i :PlugInstall<cr>
nnoremap <space><space>u :PlugUpdate<cr>
nnoremap <space><space>c :PlugClean<cr>
" 重新加载vimrc文件
nnoremap <space><space>s :source $MYVIMRC<cr>
" paste
nnoremap <leader>p "+p
vnoremap <leader>p "+p
vnoremap <leader>y "+y
nnoremap <leader>y "+y
vnoremap <leader>q :qa!  
" fast save
nnoremap <C-S> :w<cr>
inoremap <C-S> <ESC>:w<cr>
inoremap <C-C> <ESC>
nnoremap <space><space>w :xa<cr>

" QuickMotion
inoremap <C-h> <left>
inoremap <C-j> <down>
inoremap <C-k> <up>
inoremap <C-l> <right>

" insert mode as emacs
inoremap <c-a> <home>
inoremap <c-e> <end>
inoremap <c-d> <del>

" faster command mode
cnoremap <c-h> <left>
cnoremap <c-j> <down>
cnoremap <c-k> <up>
cnoremap <c-l> <right>
cnoremap <c-a> <home>
cnoremap <c-e> <end>
cnoremap <c-f> <c-d>
cnoremap <c-b> <left>
cnoremap <c-d> <del>

" 分屏窗口移动,与滚动另一页的片段
nnoremap <space>vt :vertical term<cr>
nnoremap <space>ve :Vex<cr>
tnoremap <esc> <c-\><c-n>
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l
nnoremap <space>wj <c-w>j
nnoremap <space>wk <c-w>k
nnoremap <space>wh <c-w>h
nnoremap <space>wl <c-w>l
nnoremap <space>wq <c-w>q
nnoremap <space>wo :only<cr>
noremap <M-u> <C-w>p<C-u><C-w>p
noremap <M-u> <C-w>p<C-u><C-w>p

nnoremap <space>yd :call QuickRun()<CR>
nnoremap <space>ycd :call CMakeRelease()<CR>
nnoremap <space>ycb :call Build()<CR>

if v:version > 800
  set diffopt+=indent-heuristic
endif

if &diff
    "set noscrollbind                   "不自动对齐kk
    nnoremap <space>gl :diffget LOCAL<cr>
    nnoremap <space>gb :diffget BASE<cr>
    nnoremap <space>gr :diffget REMOTE<cr>
    nnoremap <space>gu :diffupdate<cr>
    nmap <space>[ [c
    nmap <space>] ]c
endif

nnoremap <space>qs :w !sudo sh -c "cat > %"<cr>
nnoremap <space>qf :call OpenQuickfix(6)<cr>
"** 遍历整个目录 
"command! -nargs=1 FindText noautocmd vimgrep /<args>/ **/*.* | cwindow
command! -nargs=1 VimGrep noautocmd vimgrep /<args>/ ./**
command! -nargs=1 NativeGrep noautocmd grep <args> ./**
nnoremap <space>qg :VimGrep<space>
nnoremap <space>qn :NativeGrep<space>
nnoremap <C-j> :cn<CR>
nnoremap <C-k> :cp<CR>

nnoremap <space>1 1gt<CR>
nnoremap <space>2 2gt<CR>
nnoremap <space>3 3gt<CR>
nnoremap <space>4 4gt<CR>
nnoremap <space>5 5gt<CR>
nnoremap <space>6 6gt<CR>
nnoremap <space>7 7gt<CR>
nnoremap <space>8 8gt<CR>
nnoremap <space>9 8gt<CR>
nnoremap <space>tn :tabnew<space>
nnoremap <space>tc :tabclose!<cr>
nnoremap <space>tt :tab term<cr>

function! OpenQuickfix(size, ...)
    let l:mode = (a:0 == 0)? 2 : (a:1)
    function! s:WindowCheck(mode)
        if &buftype == 'quickfix'
            let s:quickfix_open = 1
            return
        endif
        if a:mode == 0
            let w:quickfix_save = winsaveview()
        else
            if exists('w:quickfix_save')
                call winrestview(w:quickfix_save)
                unlet w:quickfix_save
            endif
        endif
    endfunc
    let s:quickfix_open = 0
    let l:winnr = winnr()
    keepalt noautocmd windo call s:WindowCheck(0)
    keepalt noautocmd silent! exec ''.l:winnr.'wincmd w'
    if l:mode == 0
        if s:quickfix_open != 0
            silent! cclose
        endif
    elseif l:mode == 1
        if s:quickfix_open == 0
            keepalt exec 'botright copen '. ((a:size > 0)? a:size : ' ')
            keepalt wincmd k
        endif
    elseif l:mode == 2
        if s:quickfix_open == 0
            keepalt exec 'botright copen '. ((a:size > 0)? a:size : ' ')
            keepalt wincmd k
        else
            silent! cclose
        endif
    endif
    keepalt noautocmd windo call s:WindowCheck(1)
    keepalt noautocmd silent! exec ''.l:winnr.'wincmd w'
endfunc

function! QuickRun()
    exec 'w'
    exec "cd %:p:h"
    if &filetype == 'c'
        if has('unix')
            ! clang -g % && ./a.out && rm a.out
        else
            ! clang -g % -o a.exe && a.exe
        endif
    elseif &filetype == 'cpp'
        if has('unix')
            ! clang++ -g % && ./a.out && rm a.out
        else
            ! clang++ -g % -o a.exe && a.exe
        endif
        " elseif &filetype == 'html' || &filetype == 'htmldjango'
        "     call BrowserOpen(expand("%:p"))
        "     BLReloadPage
        " elseif &filetype == 'markdown'
        "     MarkdownPreview
        " elseif &filetype == 'tex'
        "     VimtexCompile
    elseif &filetype == 'java'
        ! javac % && java %:r && rm %:r.class
    elseif &filetype == 'javascript'
        ! node %
    elseif &filetype == 'python'
        if has("unix")
            ! python3 %
        else
            ! python %
        endif
    elseif &filetype == 'rust'
        ! cargo run
    elseif &filetype == 'sh'
        ! bash %
    elseif &filetype == 'typescript'
        ! tsc %
    else
        echo "Not supported filetype:" . " " . &filetype
    endif
endfunction

function! CMakeRelease()
    exec "w"
    if &filetype == 'c' || &filetype == 'cpp'
        if has('win32') || has('win16') || has('win64') || has('win95')
            !del /F /S /Q .\\compile_commands.json && rd /s /q .\\build && cmake -H.  -Bbuild -GNinja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=TRUE && copy .\\build\\compile_commands.json .\\ 
        else
            !rm ./compile_commands.json &&rm ./build -rf && cmake -H.  -Bbuild -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=TRUE  && ln -s ./build/compile_commands.json ./ 
        endif
    endif
endfunction

function! Build()
    exec "w"
    if &filetype == 'c' || &filetype == 'cpp'
        if has('win32') || has('win16') || has('win64') || has('win95')
            !cmake -H.  -Bbuild -GNinja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=TRUE && cmake --build build 
        else
            !cmake -H.  -Bbuild -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=TRUE  && cmake --build build 
        endif
    endif
endfunction

