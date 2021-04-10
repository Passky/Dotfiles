"----------------AsyncRun--------------------
" 自动打开 quickfix window ，高度为 6
let g:asyncrun_open = 6
" 任务结束时候响铃提醒
let g:asyncrun_rootmarks = ['.ccls','.svn', '.git', '.root', '_darcs', 'build.xml'] 

"RootDirecory
let g:rooter_patterns = ['Rakefile','.git','.git/','.svn/','.root']
let g:rooter_change_directory_for_non_project_files = 'current'
let g:rooter_silent_chdir = 1
let g:rooter_resolve_links = 1

nnoremap <space>qcd :call AsyncCMakeRelease()<cr>
nnoremap <space>qcb :call AsyncBuild()<cr>
nnoremap <space>qq :call ToasyncRun()<CR>
function! ToasyncRun()
    exec 'w'
    if &filetype == 'c'
        if has('unix')
            AsyncRun clang -g % && ./a.out && rm a.out
        else
            AsyncRun clang -g "$(VIM_FILEPATH)" -o "a.exe" && "$(VIM_FILEDIR)/a.exe"
        endif
    elseif &filetype == 'cpp'
        if has('unix')
            AsyncRun clang++ -g % && ./a.out && rm a.out
        else
            AsyncRun clang++ -g "$(VIM_FILEPATH)" -o "a.exe" && "$(VIM_FILEDIR)\a.exe"
        endif
    elseif &filetype == 'html' || &filetype == 'htmldjango'
        call BrowserOpen(expand("%:p"))
        BLReloadPage
    elseif &filetype == 'java'
        AsyncRun javac % && java %:r && rm %:r.class
    elseif &filetype == 'javascript'
        AsyncRun node %
    elseif &filetype == 'markdown'
        MarkdownPreview
    elseif &filetype == 'python'
        if has("unix")
            AsyncRun -raw python3 %
        else
            AsyncRun -raw python "$(VIM_FILEPATH)"
        endif
    elseif &filetype == 'rust'
        AsyncRun cargo run
    elseif &filetype == 'sh'
        AsyncRun bash %
    elseif &filetype == 'tex'
        VimtexCompile
    elseif &filetype == 'typescript'
        AsyncRun tsc % && node %:r.js
    else
        echo "Not supported filetype:" . " " . &filetype
    endif
endfunction

function! AsyncCMakeRelease()
    exec "w"
    if &filetype == 'c' || &filetype == 'cpp'
        if has('win32') || has('win16') || has('win64') || has('win95')
            exec "AsyncRun -cwd=<root> -raw -mode=4 del /F /S /Q .\\compile_commands.json && rd /s /q .\\build && cmake -H.  -Bbuild -GNinja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=TRUE && copy .\\build\\compile_commands.json .\\ "
        else
            exec "AsyncRun -cwd=<root> rm ./compile_commands.json &&rm ./build -rf && cmake -H.  -Bbuild -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=TRUE  && ln -s ./build/compile_commands.json ./ "
        endif
    endif
endfunction

function! AsyncBuild()
    exec "w"
    if &filetype == 'c' || &filetype == 'cpp'
        if has('win32') || has('win16') || has('win64') || has('win95')
            exec "AsyncRun -cwd=<root> -mode=4 cmake -H.  -Bbuild -GNinja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=TRUE && cmake --build build "
        else
            exec "AsyncRun -cwd=<root> -raw cmake -H.  -Bbuild -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=TRUE  && cmake --build build "
        endif
    endif
endfunction

