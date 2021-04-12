rem This was called batch file
@ECHO OFF
SETLOCAL
set edump=%HOME%\.emacs.d\emacs.pdmp
cd %HOME%
runemacs --dump-file C:\app\passky\.emacs.d\emacs.pdmp
