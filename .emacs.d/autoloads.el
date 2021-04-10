(setq load-path '("/home/passky/.emacs.d/elpa/aggressive-indent-20200824.2352" "/home/passky/.emacs.d/elpa/auto-package-update-20210211.2036" "/home/passky/.emacs.d/elpa/auto-yasnippet-20191015.942" "/home/passky/.emacs.d/elpa/benchmark-init-20150905.938" "/home/passky/.emacs.d/elpa/bing-dict-20200216.110" "/home/passky/.emacs.d/elpa/buffer-move-20160615.1803" "/home/passky/.emacs.d/elpa/cargo-20210327.1821" "/home/passky/.emacs.d/elpa/ccls-20200820.308" "/home/passky/.emacs.d/elpa/cider-20210408.1212" "/home/passky/.emacs.d/elpa/clojure-mode-20210322.704" "/home/passky/.emacs.d/elpa/cmake-mode-20210104.1831" "/home/passky/.emacs.d/elpa/command-log-mode-20160413.447" "/home/passky/.emacs.d/elpa/company-box-20210330.1155" "/home/passky/.emacs.d/elpa/company-prescient-20210227.600" "/home/passky/.emacs.d/elpa/company-quickhelp-terminal-20200904.305" "/home/passky/.emacs.d/elpa/company-quickhelp-20210329.1602" "/home/passky/.emacs.d/elpa/consult-20210405.251" "/home/passky/.emacs.d/elpa/counsel-etags-20210226.1218" "/home/passky/.emacs.d/elpa/counsel-20210404.1716" "/home/passky/.emacs.d/elpa/cpputils-cmake-20181006.328" "/home/passky/.emacs.d/elpa/csharp-mode-20210328.2004" "/home/passky/.emacs.d/elpa/csv-mode-1.14" "/home/passky/.emacs.d/elpa/dante-20210301.1738" "/home/passky/.emacs.d/elpa/dash-functional-20210210.1449" "/home/passky/.emacs.d/elpa/diff-hl-20210405.1131" "/home/passky/.emacs.d/elpa/doom-20180301.2308" "/home/passky/.emacs.d/elpa/doom-modeline-20210330.1522" "/home/passky/.emacs.d/elpa/all-the-icons-20210409.826" "/home/passky/.emacs.d/elpa/doom-themes-20210322.1750" "/home/passky/.emacs.d/elpa/dumb-jump-20210303.1714" "/home/passky/.emacs.d/elpa/edit-indirect-20200805.1840" "/home/passky/.emacs.d/elpa/elpy-20210328.1852" "/home/passky/.emacs.d/elpa/company-20210405.2354" "/home/passky/.emacs.d/elpa/embark-20210328.1709" "/home/passky/.emacs.d/elpa/eshell-z-20191116.333" "/home/passky/.emacs.d/elpa/esup-20200814.1400" "/home/passky/.emacs.d/elpa/evil-avy-20150908.748" "/home/passky/.emacs.d/elpa/evil-collection-20210401.1012" "/home/passky/.emacs.d/elpa/annalist-20190929.207" "/home/passky/.emacs.d/elpa/evil-mark-replace-20200630.940" "/home/passky/.emacs.d/elpa/evil-matchit-20210325.123" "/home/passky/.emacs.d/elpa/evil-nerd-commenter-20210311.37" "/home/passky/.emacs.d/elpa/evil-paredit-20150413.2048" "/home/passky/.emacs.d/elpa/evil-surround-20200603.2216" "/home/passky/.emacs.d/elpa/evil-20210404.2226" "/home/passky/.emacs.d/elpa/exec-path-from-shell-20201215.33" "/home/passky/.emacs.d/elpa/find-file-in-project-20210323.118" "/home/passky/.emacs.d/elpa/flycheck-20210321.852" "/home/passky/.emacs.d/elpa/flymake-racket-20210105.606" "/home/passky/.emacs.d/elpa/frame-local-20180330.940" "/home/passky/.emacs.d/elpa/geiser-20210405.2206" "/home/passky/.emacs.d/elpa/git-timemachine-20200603.701" "/home/passky/.emacs.d/elpa/gitconfig-mode-20180318.1956" "/home/passky/.emacs.d/elpa/gnu-elpa-keyring-update-2019.3" "/home/passky/.emacs.d/elpa/go-mode-20210201.1458" "/home/passky/.emacs.d/elpa/goto-chg-20201101.1029" "/home/passky/.emacs.d/elpa/groovy-mode-20191031.2256" "/home/passky/.emacs.d/elpa/haml-mode-20190219.2102" "/home/passky/.emacs.d/elpa/haskell-mode-20210325.2112" "/home/passky/.emacs.d/elpa/highlight-indentation-20210221.1418" "/home/passky/.emacs.d/elpa/hl-todo-20210117.1140" "/home/passky/.emacs.d/elpa/icomplete-vertical-20210227.2146" "/home/passky/.emacs.d/elpa/iedit-20210402.854" "/home/passky/.emacs.d/elpa/ivy-posframe-20210122.45" "/home/passky/.emacs.d/elpa/jade-mode-20160525.1441" "/home/passky/.emacs.d/elpa/kana-20201012.1415" "/home/passky/.emacs.d/elpa/lcr-20210102.853" "/home/passky/.emacs.d/elpa/liberime-20201106.858" "/home/passky/.emacs.d/elpa/load-bash-alias-20201229.1711" "/home/passky/.emacs.d/elpa/lsp-java-20210309.1856" "/home/passky/.emacs.d/elpa/dap-mode-20210405.1739" "/home/passky/.emacs.d/elpa/bui-20210108.1141" "/home/passky/.emacs.d/elpa/lsp-pyright-20210220.1714" "/home/passky/.emacs.d/elpa/lsp-treemacs-20210216.1250" "/home/passky/.emacs.d/elpa/lsp-ui-20210330.428" "/home/passky/.emacs.d/elpa/lsp-mode-20210405.1703" "/home/passky/.emacs.d/elpa/lua-mode-20201110.1250" "/home/passky/.emacs.d/elpa/magit-20210406.454" "/home/passky/.emacs.d/elpa/git-commit-20210328.1730" "/home/passky/.emacs.d/elpa/markdown-toc-20200517.1233" "/home/passky/.emacs.d/elpa/markdown-mode-20210405.1349" "/home/passky/.emacs.d/elpa/mmm-mode-20200908.2236" "/home/passky/.emacs.d/elpa/modern-cpp-font-lock-20210405.1155" "/home/passky/.emacs.d/elpa/modus-themes-20210406.551" "/home/passky/.emacs.d/elpa/ob-go-20190201.2040" "/home/passky/.emacs.d/elpa/ob-rust-20210204.244" "/home/passky/.emacs.d/elpa/org-re-reveal-20210405.1309" "/home/passky/.emacs.d/elpa/htmlize-20200816.746" "/home/passky/.emacs.d/elpa/page-break-lines-20210104.2224" "/home/passky/.emacs.d/elpa/paredit-20191121.2328" "/home/passky/.emacs.d/elpa/parseedn-20200419.1124" "/home/passky/.emacs.d/elpa/parseclj-20201012.712" "/home/passky/.emacs.d/elpa/a-20201203.1927" "/home/passky/.emacs.d/elpa/pdf-view-restore-20190904.1708" "/home/passky/.emacs.d/elpa/pdf-tools-20200512.1524" "/home/passky/.emacs.d/elpa/pkg-info-20150517.1143" "/home/passky/.emacs.d/elpa/epl-20180205.2049" "/home/passky/.emacs.d/elpa/prescient-20210227.600" "/home/passky/.emacs.d/elpa/pyim-20210319.1102" "/home/passky/.emacs.d/elpa/async-20210117.718" "/home/passky/.emacs.d/elpa/pyim-basedict-20210311.159" "/home/passky/.emacs.d/elpa/pyvenv-20201227.1623" "/home/passky/.emacs.d/elpa/queue-0.2" "/home/passky/.emacs.d/elpa/quickrun-20210330.654" "/home/passky/.emacs.d/elpa/racket-mode-20210328.2038" "/home/passky/.emacs.d/elpa/pos-tip-20191227.1356" "/home/passky/.emacs.d/elpa/rainbow-delimiters-20200827.321" "/home/passky/.emacs.d/elpa/request-20210214.37" "/home/passky/.emacs.d/elpa/rime-20210207.1432" "/home/passky/.emacs.d/elpa/popup-20210317.138" "/home/passky/.emacs.d/elpa/ripgrep-20190215.841" "/home/passky/.emacs.d/elpa/rjsx-mode-20200120.1446" "/home/passky/.emacs.d/elpa/js2-mode-20201220.1718" "/home/passky/.emacs.d/elpa/rust-mode-20210226.1106" "/home/passky/.emacs.d/elpa/scss-mode-20180123.1708" "/home/passky/.emacs.d/elpa/sesman-20190909.1754" "/home/passky/.emacs.d/elpa/shrink-path-20190208.1335" "/home/passky/.emacs.d/elpa/f-20191110.1357" "/home/passky/.emacs.d/elpa/sly-20210303.1148" "/home/passky/.emacs.d/elpa/smartparens-20210330.850" "/home/passky/.emacs.d/elpa/spinner-1.7.3" "/home/passky/.emacs.d/elpa/ssass-mode-20200211.132" "/home/passky/.emacs.d/elpa/super-save-20200930.1634" "/home/passky/.emacs.d/elpa/swiper-20210404.1302" "/home/passky/.emacs.d/elpa/ivy-20210404.1241" "/home/passky/.emacs.d/elpa/tablist-20200427.2205" "/home/passky/.emacs.d/elpa/toc-org-20210323.1256" "/home/passky/.emacs.d/elpa/transient-20210315.1902" "/home/passky/.emacs.d/elpa/tree-sitter-indent-20210322.2033" "/home/passky/.emacs.d/elpa/tree-sitter-langs-20210314.1704" "/home/passky/.emacs.d/elpa/tree-sitter-20210328.434" "/home/passky/.emacs.d/elpa/treemacs-20210331.1948" "/home/passky/.emacs.d/elpa/cfrs-20210217.1848" "/home/passky/.emacs.d/elpa/posframe-20210331.324" "/home/passky/.emacs.d/elpa/ht-20210119.741" "/home/passky/.emacs.d/elpa/hydra-20201115.1055" "/home/passky/.emacs.d/elpa/lv-20200507.1518" "/home/passky/.emacs.d/elpa/pfuture-20200425.1357" "/home/passky/.emacs.d/elpa/ace-window-20200606.1259" "/home/passky/.emacs.d/elpa/avy-20201226.1734" "/home/passky/.emacs.d/elpa/s-20180406.808" "/home/passky/.emacs.d/elpa/dash-20210330.1544" "/home/passky/.emacs.d/elpa/tsc-20210320.1052" "/home/passky/.emacs.d/elpa/typescript-mode-20201002.1109" "/home/passky/.emacs.d/elpa/undo-fu-20200701.1435" "/home/passky/.emacs.d/elpa/use-package-20210207.1926" "/home/passky/.emacs.d/elpa/bind-key-20210210.1609" "/home/passky/.emacs.d/elpa/valign-3.0.0" "/home/passky/.emacs.d/elpa/vimrc-mode-20181116.1919" "/home/passky/.emacs.d/elpa/volatile-highlights-20160612.155" "/home/passky/.emacs.d/elpa/web-mode-20210131.1758" "/home/passky/.emacs.d/elpa/wgrep-20210322.2207" "/home/passky/.emacs.d/elpa/which-key-20210324.1821" "/home/passky/.emacs.d/elpa/with-editor-20210319.1930" "/home/passky/.emacs.d/elpa/writeroom-mode-20201229.2242" "/home/passky/.emacs.d/elpa/visual-fill-column-20210404.2152" "/home/passky/.emacs.d/elpa/wucuo-20210316.156" "/home/passky/.emacs.d/elpa/xclip-1.10" "/home/passky/.emacs.d/elpa/xr-1.20" "/home/passky/.emacs.d/elpa/yaml-mode-20201109.1026" "/home/passky/.emacs.d/elpa/yapfify-20200406.830" "/home/passky/.emacs.d/elpa/yasnippet-snippets-20210105.1346" "/home/passky/.emacs.d/elpa/yasnippet-20200604.246" "~/.emacs.d/manual-package" "/home/passky/.emacs.d/manual-package/emacs-application-framework" "/home/passky/.emacs.d/manual-package/wm" "/usr/share/emacs/28.0.50/site-lisp" "/usr/share/emacs/site-lisp" "/usr/share/emacs/28.0.50/lisp" "/usr/share/emacs/28.0.50/lisp/vc" "/usr/share/emacs/28.0.50/lisp/url" "/usr/share/emacs/28.0.50/lisp/textmodes" "/usr/share/emacs/28.0.50/lisp/progmodes" "/usr/share/emacs/28.0.50/lisp/play" "/usr/share/emacs/28.0.50/lisp/org" "/usr/share/emacs/28.0.50/lisp/nxml" "/usr/share/emacs/28.0.50/lisp/net" "/usr/share/emacs/28.0.50/lisp/mh-e" "/usr/share/emacs/28.0.50/lisp/mail" "/usr/share/emacs/28.0.50/lisp/leim" "/usr/share/emacs/28.0.50/lisp/language" "/usr/share/emacs/28.0.50/lisp/international" "/usr/share/emacs/28.0.50/lisp/image" "/usr/share/emacs/28.0.50/lisp/gnus" "/usr/share/emacs/28.0.50/lisp/eshell" "/usr/share/emacs/28.0.50/lisp/erc" "/usr/share/emacs/28.0.50/lisp/emulation" "/usr/share/emacs/28.0.50/lisp/emacs-lisp" "/usr/share/emacs/28.0.50/lisp/cedet" "/usr/share/emacs/28.0.50/lisp/calendar" "/usr/share/emacs/28.0.50/lisp/calc" "/usr/share/emacs/28.0.50/lisp/obsolete") auto-mode-alist '(("README\\(?:\\.md\\)?\\'" . gfm-mode) ("/cgdbrc\\'" . conf-space-mode) ("\\.gitignore\\'" . conf-unix-mode) ("\\.clj\\'" . clojure-mode) ("\\.class\\'" . hexl-mode) ("\\.exe\\'" . hexl-mode) ("\\.out\\'" . hexl-mode) ("\\.o\\'" . hexl-mode) ("\\.gradle\\'" . groovy-mode) ("\\.groovy\\'" . groovy-mode) ("\\.ctags\\'" . text-mode) ("TAGS\\'" . text-mode) ("\\.?vim\\(rc\\)?$" . vimrc-mode) ("\\.env$" . sh-mode) ("\\.z?sh$" . sh-mode) ("\\.bash\\(_profile\\|_history\\|rc\\.local\\|rc\\)?$" . sh-mode) ("\\.tsx\\'" . typescript-mode) ("\\.ts$" . typescript-mode) ("\\.babelrc\\'" . js-mode) ("\\.mock.js\\'" . js-mode) ("components\\/.*\\.js\\'" . rjsx-mode) ("\\.js\\(\\.erb\\)?\\'" . js2-mode) ("\\.vue\\'" . web-mode) ("\\.html\\'" . web-mode) ("\\.ninja$" . java-mode) ("\\.aj\\'" . java-mode) ("\\CMakeLists.txt\\'" . cmake-mode) ("\\.cxx\\'" . cc-mode) ("\\.cxx\\'" . c++-mode) ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode) ("\\.cljs\\'" . clojurescript-mode) ("\\.cljc\\'" . clojurec-mode) ("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode) ("\\.cmake\\'" . cmake-mode) ("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cs\\'" . csharp-mode) ("\\.tsv\\'" . tsv-mode) ("\\.[Cc][Ss][Vv]\\'" . csv-mode) ("/etc/gitconfig\\'" . gitconfig-mode) ("/\\.gitmodules\\'" . gitconfig-mode) ("/git/config\\'" . gitconfig-mode) ("/modules/.*/config\\'" . gitconfig-mode) ("/\\.git/config\\'" . gitconfig-mode) ("/\\.gitconfig\\'" . gitconfig-mode) ("go\\.mod\\'" . go-dot-mod-mode) ("\\.go\\'" . go-mode) ("Jenkinsfile" . groovy-mode) ("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" . groovy-mode) ("\\.haml\\'" . haml-mode) ("\\.hsc\\'" . haskell-mode) ("\\.l[gh]s\\'" . haskell-literate-mode) ("\\.hsig\\'" . haskell-mode) ("\\.[gh]s\\'" . haskell-mode) ("\\.cabal\\'\\|/cabal\\.project\\|/\\.cabal/config\\'" . haskell-cabal-mode) ("\\.chs\\'" . haskell-c2hs-mode) ("\\.ghci\\'" . ghci-script-mode) ("\\.dump-simpl\\'" . ghc-core-mode) ("\\.hcr\\'" . ghc-core-mode) ("\\.pug\\'" . jade-mode) ("\\.jade\\'" . jade-mode) ("\\.lua\\'" . lua-mode) ("/git-rebase-todo\\'" . git-rebase-mode) ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode) ("\\.rktl\\'" . racket-mode) ("\\.rktd\\'" . racket-mode) ("\\.rkt\\'" . racket-mode) ("\\.jsx\\'" . rjsx-mode) ("\\.rs\\'" . rust-mode) ("\\.ts\\'" . typescript-mode) ("\\.exrc\\'" . vimrc-mode) ("[._]?g?vimrc\\'" . vimrc-mode) ("\\.vim\\'" . vimrc-mode) ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode) ("\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" nil epa-file) ("\\.elc\\'" . elisp-byte-code-mode) ("\\.zst\\'" nil jka-compr) ("\\.dz\\'" nil jka-compr) ("\\.xz\\'" nil jka-compr) ("\\.lzma\\'" nil jka-compr) ("\\.lz\\'" nil jka-compr) ("\\.g?z\\'" nil jka-compr) ("\\.bz2\\'" nil jka-compr) ("\\.Z\\'" nil jka-compr) ("\\.vr[hi]?\\'" . vera-mode) ("\\(?:\\.\\(?:rbw?\\|ru\\|rake\\|thor\\|jbuilder\\|rabl\\|gemspec\\|podspec\\)\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Puppet\\|Berks\\|Brew\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode) ("\\.re?st\\'" . rst-mode) ("\\.py[iw]?\\'" . python-mode) ("\\.m\\'" . octave-maybe-mode) ("\\.less\\'" . less-css-mode) ("\\.scss\\'" . scss-mode) ("\\.awk\\'" . awk-mode) ("\\.\\(u?lpc\\|pike\\|pmod\\(\\.in\\)?\\)\\'" . pike-mode) ("\\.idl\\'" . idl-mode) ("\\.java\\'" . java-mode) ("\\.m\\'" . objc-mode) ("\\.ii\\'" . c++-mode) ("\\.i\\'" . c-mode) ("\\.lex\\'" . c-mode) ("\\.y\\(acc\\)?\\'" . c-mode) ("\\.h\\'" . c-or-c++-mode) ("\\.c\\'" . c-mode) ("\\.\\(CC?\\|HH?\\)\\'" . c++-mode) ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode) ("\\.\\(cc\\|hh\\)\\'" . c++-mode) ("\\.\\(bat\\|cmd\\)\\'" . bat-mode) ("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . mhtml-mode) ("\\.svgz?\\'" . image-mode) ("\\.svgz?\\'" . xml-mode) ("\\.x[bp]m\\'" . image-mode) ("\\.x[bp]m\\'" . c-mode) ("\\.p[bpgn]m\\'" . image-mode) ("\\.tiff?\\'" . image-mode) ("\\.gif\\'" . image-mode) ("\\.png\\'" . image-mode) ("\\.jpe?g\\'" . image-mode) ("\\.te?xt\\'" . text-mode) ("\\.[tT]e[xX]\\'" . tex-mode) ("\\.ins\\'" . tex-mode) ("\\.ltx\\'" . latex-mode) ("\\.dtx\\'" . doctex-mode) ("\\.org\\'" . org-mode) ("\\.dir-locals\\(?:-2\\)?\\.el\\'" . lisp-data-mode) ("eww-bookmarks\\'" . lisp-data-mode) ("tramp\\'" . lisp-data-mode) ("/archive-contents\\'" . lisp-data-mode) ("places\\'" . lisp-data-mode) ("\\.emacs-places\\'" . lisp-data-mode) ("\\.el\\'" . emacs-lisp-mode) ("Project\\.ede\\'" . emacs-lisp-mode) ("\\.\\(scm\\|stk\\|ss\\|sch\\)\\'" . scheme-mode) ("\\.l\\'" . lisp-mode) ("\\.li?sp\\'" . lisp-mode) ("\\.[fF]\\'" . fortran-mode) ("\\.for\\'" . fortran-mode) ("\\.p\\'" . pascal-mode) ("\\.pas\\'" . pascal-mode) ("\\.\\(dpr\\|DPR\\)\\'" . delphi-mode) ("\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'" . perl-mode) ("Imakefile\\'" . makefile-imake-mode) ("Makeppfile\\(?:\\.mk\\)?\\'" . makefile-makepp-mode) ("\\.makepp\\'" . makefile-makepp-mode) ("\\.mk\\'" . makefile-gmake-mode) ("\\.make\\'" . makefile-gmake-mode) ("[Mm]akefile\\'" . makefile-gmake-mode) ("\\.am\\'" . makefile-automake-mode) ("\\.texinfo\\'" . texinfo-mode) ("\\.te?xi\\'" . texinfo-mode) ("\\.[sS]\\'" . asm-mode) ("\\.asm\\'" . asm-mode) ("\\.css\\'" . css-mode) ("\\.mixal\\'" . mixal-mode) ("\\.gcov\\'" . compilation-mode) ("/\\.[a-z0-9-]*gdbinit" . gdb-script-mode) ("-gdb\\.gdb" . gdb-script-mode) ("[cC]hange\\.?[lL]og?\\'" . change-log-mode) ("[cC]hange[lL]og[-.][0-9]+\\'" . change-log-mode) ("\\$CHANGE_LOG\\$\\.TXT" . change-log-mode) ("\\.scm\\.[0-9]*\\'" . scheme-mode) ("\\.[ckz]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\'" . sh-mode) ("\\.bash\\'" . sh-mode) ("/PKGBUILD\\'" . sh-mode) ("\\(/\\|\\`\\)\\.\\(bash_\\(profile\\|history\\|log\\(in\\|out\\)\\)\\|z?log\\(in\\|out\\)\\)\\'" . sh-mode) ("\\(/\\|\\`\\)\\.\\(shrc\\|zshrc\\|m?kshrc\\|bashrc\\|t?cshrc\\|esrc\\)\\'" . sh-mode) ("\\(/\\|\\`\\)\\.\\([kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'" . sh-mode) ("\\.m?spec\\'" . sh-mode) ("\\.m[mes]\\'" . nroff-mode) ("\\.man\\'" . nroff-mode) ("\\.sty\\'" . latex-mode) ("\\.cl[so]\\'" . latex-mode) ("\\.bbl\\'" . latex-mode) ("\\.bib\\'" . bibtex-mode) ("\\.bst\\'" . bibtex-style-mode) ("\\.sql\\'" . sql-mode) ("\\(acinclude\\|aclocal\\|acsite\\)\\.m4\\'" . autoconf-mode) ("\\.m[4c]\\'" . m4-mode) ("\\.mf\\'" . metafont-mode) ("\\.mp\\'" . metapost-mode) ("\\.vhdl?\\'" . vhdl-mode) ("\\.article\\'" . text-mode) ("\\.letter\\'" . text-mode) ("\\.i?tcl\\'" . tcl-mode) ("\\.exp\\'" . tcl-mode) ("\\.itk\\'" . tcl-mode) ("\\.icn\\'" . icon-mode) ("\\.sim\\'" . simula-mode) ("\\.mss\\'" . scribe-mode) ("\\.f9[05]\\'" . f90-mode) ("\\.f0[38]\\'" . f90-mode) ("\\.indent\\.pro\\'" . fundamental-mode) ("\\.\\(pro\\|PRO\\)\\'" . idlwave-mode) ("\\.srt\\'" . srecode-template-mode) ("\\.prolog\\'" . prolog-mode) ("\\.tar\\'" . tar-mode) ("\\.\\(arc\\|zip\\|lzh\\|lha\\|zoo\\|[jew]ar\\|xpi\\|rar\\|cbr\\|7z\\|squashfs\\|ARC\\|ZIP\\|LZH\\|LHA\\|ZOO\\|[JEW]AR\\|XPI\\|RAR\\|CBR\\|7Z\\|SQUASHFS\\)\\'" . archive-mode) ("\\.oxt\\'" . archive-mode) ("\\.\\(deb\\|[oi]pk\\)\\'" . archive-mode) ("\\`/tmp/Re" . text-mode) ("/Message[0-9]*\\'" . text-mode) ("\\`/tmp/fol/" . text-mode) ("\\.oak\\'" . scheme-mode) ("\\.sgml?\\'" . sgml-mode) ("\\.x[ms]l\\'" . xml-mode) ("\\.dbk\\'" . xml-mode) ("\\.dtd\\'" . sgml-mode) ("\\.ds\\(ss\\)?l\\'" . dsssl-mode) ("\\.js[mx]?\\'" . javascript-mode) ("\\.har\\'" . javascript-mode) ("\\.json\\'" . javascript-mode) ("\\.[ds]?va?h?\\'" . verilog-mode) ("\\.by\\'" . bovine-grammar-mode) ("\\.wy\\'" . wisent-grammar-mode) ("[:/\\]\\..*\\(emacs\\|gnus\\|viper\\)\\'" . emacs-lisp-mode) ("\\`\\..*emacs\\'" . emacs-lisp-mode) ("[:/]_emacs\\'" . emacs-lisp-mode) ("/crontab\\.X*[0-9]+\\'" . shell-script-mode) ("\\.ml\\'" . lisp-mode) ("\\.ld[si]?\\'" . ld-script-mode) ("ld\\.?script\\'" . ld-script-mode) ("\\.xs\\'" . c-mode) ("\\.x[abdsru]?[cnw]?\\'" . ld-script-mode) ("\\.zone\\'" . dns-mode) ("\\.soa\\'" . dns-mode) ("\\.asd\\'" . lisp-mode) ("\\.\\(asn\\|mib\\|smi\\)\\'" . snmp-mode) ("\\.\\(as\\|mi\\|sm\\)2\\'" . snmpv2-mode) ("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode) ("\\.\\(dif\\|pat\\)\\'" . diff-mode) ("\\.[eE]?[pP][sS]\\'" . ps-mode) ("\\.\\(?:PDF\\|DVI\\|OD[FGPST]\\|DOCX\\|XLSX?\\|PPTX?\\|pdf\\|djvu\\|dvi\\|od[fgpst]\\|docx\\|xlsx?\\|pptx?\\)\\'" . doc-view-mode-maybe) ("configure\\.\\(ac\\|in\\)\\'" . autoconf-mode) ("\\.s\\(v\\|iv\\|ieve\\)\\'" . sieve-mode) ("BROWSE\\'" . ebrowse-tree-mode) ("\\.ebrowse\\'" . ebrowse-tree-mode) ("#\\*mail\\*" . mail-mode) ("\\.g\\'" . antlr-mode) ("\\.mod\\'" . m2-mode) ("\\.ses\\'" . ses-mode) ("\\.docbook\\'" . sgml-mode) ("\\.com\\'" . dcl-mode) ("/config\\.\\(?:bat\\|log\\)\\'" . fundamental-mode) ("/\\.\\(authinfo\\|netrc\\)\\'" . authinfo-mode) ("\\.\\(?:[iI][nN][iI]\\|[lL][sS][tT]\\|[rR][eE][gG]\\|[sS][yY][sS]\\)\\'" . conf-mode) ("\\.la\\'" . conf-unix-mode) ("\\.ppd\\'" . conf-ppd-mode) ("java.+\\.conf\\'" . conf-javaprop-mode) ("\\.properties\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'" . conf-javaprop-mode) ("\\.toml\\'" . conf-toml-mode) ("\\.desktop\\'" . conf-desktop-mode) ("/\\.redshift\\.conf\\'" . conf-windows-mode) ("\\`/etc/\\(?:DIR_COLORS\\|ethers\\|.?fstab\\|.*hosts\\|lesskey\\|login\\.?de\\(?:fs\\|vperm\\)\\|magic\\|mtab\\|pam\\.d/.*\\|permissions\\(?:\\.d/.+\\)?\\|protocols\\|rpc\\|services\\)\\'" . conf-space-mode) ("\\`/etc/\\(?:acpid?/.+\\|aliases\\(?:\\.d/.+\\)?\\|default/.+\\|group-?\\|hosts\\..+\\|inittab\\|ksysguarddrc\\|opera6rc\\|passwd-?\\|shadow-?\\|sysconfig/.+\\)\\'" . conf-mode) ("[cC]hange[lL]og[-.][-0-9a-z]+\\'" . change-log-mode) ("/\\.?\\(?:gitconfig\\|gnokiirc\\|hgrc\\|kde.*rc\\|mime\\.types\\|wgetrc\\)\\'" . conf-mode) ("/\\.\\(?:asound\\|enigma\\|fetchmail\\|gltron\\|gtk\\|hxplayer\\|mairix\\|mbsync\\|msmtp\\|net\\|neverball\\|nvidia-settings-\\|offlineimap\\|qt/.+\\|realplayer\\|reportbug\\|rtorrent\\.\\|screen\\|scummvm\\|sversion\\|sylpheed/.+\\|xmp\\)rc\\'" . conf-mode) ("/\\.\\(?:gdbtkinit\\|grip\\|mpdconf\\|notmuch-config\\|orbital/.+txt\\|rhosts\\|tuxracer/options\\)\\'" . conf-mode) ("/\\.?X\\(?:default\\|resource\\|re\\)s\\>" . conf-xdefaults-mode) ("/X11.+app-defaults/\\|\\.ad\\'" . conf-xdefaults-mode) ("/X11.+locale/.+/Compose\\'" . conf-colon-mode) ("/X11.+locale/compose\\.dir\\'" . conf-javaprop-mode) ("\\.~?[0-9]+\\.[0-9][-.0-9]*~?\\'" nil t) ("\\.\\(?:orig\\|in\\|[bB][aA][kK]\\)\\'" nil t) ("[/.]c\\(?:on\\)?f\\(?:i?g\\)?\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'" . conf-mode-maybe) ("\\.[1-9]\\'" . nroff-mode) ("\\.art\\'" . image-mode) ("\\.avs\\'" . image-mode) ("\\.bmp\\'" . image-mode) ("\\.cmyk\\'" . image-mode) ("\\.cmyka\\'" . image-mode) ("\\.crw\\'" . image-mode) ("\\.dcr\\'" . image-mode) ("\\.dcx\\'" . image-mode) ("\\.dng\\'" . image-mode) ("\\.dpx\\'" . image-mode) ("\\.fax\\'" . image-mode) ("\\.hrz\\'" . image-mode) ("\\.icb\\'" . image-mode) ("\\.icc\\'" . image-mode) ("\\.icm\\'" . image-mode) ("\\.ico\\'" . image-mode) ("\\.icon\\'" . image-mode) ("\\.jbg\\'" . image-mode) ("\\.jbig\\'" . image-mode) ("\\.jng\\'" . image-mode) ("\\.jnx\\'" . image-mode) ("\\.miff\\'" . image-mode) ("\\.mng\\'" . image-mode) ("\\.mvg\\'" . image-mode) ("\\.otb\\'" . image-mode) ("\\.p7\\'" . image-mode) ("\\.pcx\\'" . image-mode) ("\\.pdb\\'" . image-mode) ("\\.pfa\\'" . image-mode) ("\\.pfb\\'" . image-mode) ("\\.picon\\'" . image-mode) ("\\.pict\\'" . image-mode) ("\\.rgb\\'" . image-mode) ("\\.rgba\\'" . image-mode) ("\\.tga\\'" . image-mode) ("\\.wbmp\\'" . image-mode) ("\\.webp\\'" . image-mode) ("\\.wmf\\'" . image-mode) ("\\.wpg\\'" . image-mode) ("\\.xcf\\'" . image-mode) ("\\.xmp\\'" . image-mode) ("\\.xwd\\'" . image-mode) ("\\.yuv\\'" . image-mode) ("\\.tgz\\'" . tar-mode) ("\\.tbz2?\\'" . tar-mode) ("\\.txz\\'" . tar-mode) ("\\.tzst\\'" . tar-mode)) Info-directory-list '("/home/passky/.emacs.d/elpa/consult-20210405.251" "/home/passky/.emacs.d/elpa/embark-20210328.1709" "/home/passky/.emacs.d/elpa/annalist-20190929.207" "/home/passky/.emacs.d/elpa/evil-20210404.2226" "/home/passky/.emacs.d/elpa/geiser-20210405.2206" "/home/passky/.emacs.d/elpa/haskell-mode-20210325.2112" "/home/passky/.emacs.d/elpa/icomplete-vertical-20210227.2146" "/home/passky/.emacs.d/elpa/magit-20210406.454" "/home/passky/.emacs.d/elpa/mmm-mode-20200908.2236" "/home/passky/.emacs.d/elpa/modus-themes-20210406.551" "/home/passky/.emacs.d/elpa/racket-mode-20210328.2038" "/home/passky/.emacs.d/elpa/sly-20210303.1148" "/home/passky/.emacs.d/elpa/ivy-20210404.1241" "/home/passky/.emacs.d/elpa/transient-20210315.1902" "/home/passky/.emacs.d/elpa/dash-20210330.1544" "/home/passky/.emacs.d/elpa/use-package-20210207.1926" "/home/passky/.emacs.d/elpa/with-editor-20210319.1930" "/home/passky/.emacs.d/elpa/writeroom-mode-20201229.2242" "/usr/share/info/" "/usr/share/info/"));;; a-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "a" "a.el" (0 0 0 0))
;;; Generated autoloads from a.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "a" '("a-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; a-autoloads.el ends here
;;; ace-window-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ace-window" "ace-window.el" (0 0 0 0))
;;; Generated autoloads from ace-window.el

(autoload 'ace-select-window "ace-window" "\
Ace select window." t nil)

(autoload 'ace-delete-window "ace-window" "\
Ace delete window." t nil)

(autoload 'ace-swap-window "ace-window" "\
Ace swap window." t nil)

(autoload 'ace-delete-other-windows "ace-window" "\
Ace delete other windows." t nil)

(autoload 'ace-display-buffer "ace-window" "\
Make `display-buffer' and `pop-to-buffer' select using `ace-window'.
See sample config for `display-buffer-base-action' and `display-buffer-alist':
https://github.com/abo-abo/ace-window/wiki/display-buffer.

\(fn BUFFER ALIST)" nil nil)

(autoload 'ace-window "ace-window" "\
Select a window.
Perform an action based on ARG described below.

By default, behaves like extended `other-window'.
See `aw-scope' which extends it to work with frames.

Prefixed with one \\[universal-argument], does a swap between the
selected window and the current window, so that the selected
buffer moves to current window (and current buffer moves to
selected window).

Prefixed with two \\[universal-argument]'s, deletes the selected
window.

\(fn ARG)" t nil)

(defvar ace-window-display-mode nil "\
Non-nil if Ace-Window-Display mode is enabled.
See the `ace-window-display-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ace-window-display-mode'.")

(custom-autoload 'ace-window-display-mode "ace-window" nil)

(autoload 'ace-window-display-mode "ace-window" "\
Minor mode for showing the ace window key in the mode line.

If called interactively, enable Ace-Window-Display mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ace-window" '("ace-window-mode" "aw-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ace-window-autoloads.el ends here
;;; aggressive-indent-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "aggressive-indent" "aggressive-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from aggressive-indent.el

(autoload 'aggressive-indent-indent-defun "aggressive-indent" "\
Indent current defun.
Throw an error if parentheses are unbalanced.
If L and R are provided, use them for finding the start and end of defun.

\(fn &optional L R)" t nil)

(autoload 'aggressive-indent-indent-region-and-on "aggressive-indent" "\
Indent region between L and R, and then some.
Call `aggressive-indent-region-function' between L and R, and
then keep indenting until nothing more happens.

\(fn L R)" t nil)

(autoload 'aggressive-indent-mode "aggressive-indent" "\
Toggle Aggressive-Indent mode on or off.

If called interactively, toggle `Aggressive-Indent mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{aggressive-indent-mode-map}

\(fn &optional ARG)" t nil)

(put 'global-aggressive-indent-mode 'globalized-minor-mode t)

(defvar global-aggressive-indent-mode nil "\
Non-nil if Global Aggressive-Indent mode is enabled.
See the `global-aggressive-indent-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-aggressive-indent-mode'.")

(custom-autoload 'global-aggressive-indent-mode "aggressive-indent" nil)

(autoload 'global-aggressive-indent-mode "aggressive-indent" "\
Toggle Aggressive-Indent mode in all buffers.
With prefix ARG, enable Global Aggressive-Indent mode if ARG is
positive; otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Aggressive-Indent mode is enabled in all buffers where
`aggressive-indent-mode' would do it.

See `aggressive-indent-mode' for more information on
Aggressive-Indent mode.

\(fn &optional ARG)" t nil)

(defalias 'aggressive-indent-global-mode #'global-aggressive-indent-mode)

(register-definition-prefixes "aggressive-indent" '("aggressive-indent-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; aggressive-indent-autoloads.el ends here
;;; all-the-icons-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "all-the-icons" "all-the-icons.el" (0 0 0 0))
;;; Generated autoloads from all-the-icons.el

(autoload 'all-the-icons-icon-for-dir "all-the-icons" "\
Get the formatted icon for DIR.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

Note: You want chevron, please use `all-the-icons-icon-for-dir-with-chevron'.

\(fn DIR &rest ARG-OVERRIDES)" nil nil)

(autoload 'all-the-icons-icon-for-file "all-the-icons" "\
Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

\(fn FILE &rest ARG-OVERRIDES)" nil nil)

(autoload 'all-the-icons-icon-for-mode "all-the-icons" "\
Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

\(fn MODE &rest ARG-OVERRIDES)" nil nil)

(autoload 'all-the-icons-icon-for-url "all-the-icons" "\
Get the formatted icon for URL.
If an icon for URL isn't found in `all-the-icons-url-alist', a globe is used.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

\(fn URL &rest ARG-OVERRIDES)" nil nil)

(autoload 'all-the-icons-install-fonts "all-the-icons" "\
Helper function to download and install the latests fonts based on OS.
When PFX is non-nil, ignore the prompt and just install

\(fn &optional PFX)" t nil)

(autoload 'all-the-icons-insert "all-the-icons" "\
Interactive icon insertion function.
When Prefix ARG is non-nil, insert the propertized icon.
When FAMILY is non-nil, limit the candidates to the icon set matching it.

\(fn &optional ARG FAMILY)" t nil)

(register-definition-prefixes "all-the-icons" '("all-the-icons-"))

;;;***

;;;### (autoloads nil nil ("all-the-icons-faces.el" "all-the-icons-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; all-the-icons-autoloads.el ends here
;;; annalist-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "annalist" "annalist.el" (0 0 0 0))
;;; Generated autoloads from annalist.el

(autoload 'annalist-record "annalist" "\
In the store for ANNALIST, TYPE, and LOCAL, record RECORD.
ANNALIST should correspond to the package/user recording this information (e.g.
'general, 'me, etc.). TYPE is the type of information being recorded (e.g.
'keybindings). LOCAL corresponds to whether to store RECORD only for the current
buffer. This information together is used to select where RECORD should be
stored in and later retrieved from with `annalist-describe'. RECORD should be a
list of items to record and later print as org headings and column entries in a
single row. If PLIST is non-nil, RECORD should be a plist instead of an ordered
list (e.g. '(keymap org-mode-map key \"C-c a\" ...)). The plist keys should be
the symbols used for the definition of TYPE.

\(fn ANNALIST TYPE RECORD &key LOCAL PLIST)" nil nil)

(autoload 'annalist-describe "annalist" "\
Describe information recorded by ANNALIST for TYPE.
For example: (annalist-describe 'general 'keybindings) If VIEW is non-nil, use
those settings for displaying recorded information instead of the defaults.

\(fn ANNALIST TYPE &optional VIEW)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "annalist" '("annalist-")))

;;;***

;;;### (autoloads nil nil ("annalist-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; annalist-autoloads.el ends here
;;; async-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "async" "async.el" (0 0 0 0))
;;; Generated autoloads from async.el

(autoload 'async-start-process "async" "\
Start the executable PROGRAM asynchronously named NAME.  See `async-start'.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished.  Set DEFAULT-DIRECTORY to change PROGRAM's current
working directory.

\(fn NAME PROGRAM FINISH-FUNC &rest PROGRAM-ARGS)" nil nil)

(autoload 'async-start "async" "\
Execute START-FUNC (often a lambda) in a subordinate Emacs process.
When done, the return value is passed to FINISH-FUNC.  Example:

    (async-start
       ;; What to do in the child process
       (lambda ()
         (message \"This is a test\")
         (sleep-for 3)
         222)

       ;; What to do when it finishes
       (lambda (result)
         (message \"Async process done, result should be 222: %s\"
                  result)))

If FINISH-FUNC is nil or missing, a future is returned that can
be inspected using `async-get', blocking until the value is
ready.  Example:

    (let ((proc (async-start
                   ;; What to do in the child process
                   (lambda ()
                     (message \"This is a test\")
                     (sleep-for 3)
                     222))))

        (message \"I'm going to do some work here\") ;; ....

        (message \"Waiting on async process, result should be 222: %s\"
                 (async-get proc)))

If you don't want to use a callback, and you don't care about any
return value from the child process, pass the `ignore' symbol as
the second argument (if you don't, and never call `async-get', it
will leave *emacs* process buffers hanging around):

    (async-start
     (lambda ()
       (delete-file \"a remote file on a slow link\" nil))
     'ignore)

Note: Even when FINISH-FUNC is present, a future is still
returned except that it yields no value (since the value is
passed to FINISH-FUNC).  Call `async-get' on such a future always
returns nil.  It can still be useful, however, as an argument to
`async-ready' or `async-wait'.

\(fn START-FUNC &optional FINISH-FUNC)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "async" '("async-")))

;;;***

;;;### (autoloads nil "async-bytecomp" "async-bytecomp.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from async-bytecomp.el

(autoload 'async-byte-recompile-directory "async-bytecomp" "\
Compile all *.el files in DIRECTORY asynchronously.
All *.elc files are systematically deleted before proceeding.

\(fn DIRECTORY &optional QUIET)" nil nil)

(defvar async-bytecomp-package-mode nil "\
Non-nil if Async-Bytecomp-Package mode is enabled.
See the `async-bytecomp-package-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `async-bytecomp-package-mode'.")

(custom-autoload 'async-bytecomp-package-mode "async-bytecomp" nil)

(autoload 'async-bytecomp-package-mode "async-bytecomp" "\
Byte compile asynchronously packages installed with package.el.
Async compilation of packages can be controlled by
`async-bytecomp-allowed-packages'.

If called interactively, enable Async-Bytecomp-Package mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'async-byte-compile-file "async-bytecomp" "\
Byte compile Lisp code FILE asynchronously.

Same as `byte-compile-file' but asynchronous.

\(fn FILE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "async-bytecomp" '("async-byte")))

;;;***

;;;### (autoloads nil "dired-async" "dired-async.el" (0 0 0 0))
;;; Generated autoloads from dired-async.el

(defvar dired-async-mode nil "\
Non-nil if Dired-Async mode is enabled.
See the `dired-async-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dired-async-mode'.")

(custom-autoload 'dired-async-mode "dired-async" nil)

(autoload 'dired-async-mode "dired-async" "\
Do dired actions asynchronously.

If called interactively, enable Dired-Async mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'dired-async-do-copy "dired-async" "\
Run ‘dired-do-copy’ asynchronously.

\(fn &optional ARG)" t nil)

(autoload 'dired-async-do-symlink "dired-async" "\
Run ‘dired-do-symlink’ asynchronously.

\(fn &optional ARG)" t nil)

(autoload 'dired-async-do-hardlink "dired-async" "\
Run ‘dired-do-hardlink’ asynchronously.

\(fn &optional ARG)" t nil)

(autoload 'dired-async-do-rename "dired-async" "\
Run ‘dired-do-rename’ asynchronously.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-async" '("dired-async-")))

;;;***

;;;### (autoloads nil "smtpmail-async" "smtpmail-async.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from smtpmail-async.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smtpmail-async" '("async-smtpmail-")))

;;;***

;;;### (autoloads nil nil ("async-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; async-autoloads.el ends here
;;; auto-package-update-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "auto-package-update" "auto-package-update.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from auto-package-update.el

(autoload 'auto-package-update-now "auto-package-update" "\
Update installed Emacs packages." t nil)

(autoload 'auto-package-update-at-time "auto-package-update" "\
Try to update every day at the specified TIME.

\(fn TIME)" nil nil)

(autoload 'auto-package-update-maybe "auto-package-update" "\
Update installed Emacs packages if at least `auto-package-update-interval' days have passed since the last update." nil nil)

(register-definition-prefixes "auto-package-update" '("apu--" "auto-package-update-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-package-update-autoloads.el ends here
;;; auto-yasnippet-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "auto-yasnippet" "auto-yasnippet.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from auto-yasnippet.el

(autoload 'aya-create-one-line "auto-yasnippet" "\
A simplistic `aya-create' to create only one mirror.
You can still have as many instances of this mirror as you want.
It's less flexible than `aya-create', but faster.
It uses a different marker, which is `aya-marker-one-line'.
You can use it to quickly generate one-liners such as
menu.add_item(spamspamspam, \"spamspamspam\")" t nil)

(autoload 'aya-create "auto-yasnippet" "\
Create a snippet from the text between BEG and END.
When the bounds are not given, use either the current region or line.

Remove `aya-marker' prefixes, write the corresponding snippet to
`aya-current', with words prefixed by `aya-marker' as fields, and
mirrors properly set up.

\(fn &optional BEG END)" t nil)

(autoload 'aya-expand "auto-yasnippet" "\
Insert the last yasnippet created by `aya-create'." t nil)

(autoload 'aya-open-line "auto-yasnippet" "\
Call `open-line', unless there are abbrevs or snippets at point.
In that case expand them.  If there's a snippet expansion in progress,
move to the next field.  Call `open-line' if nothing else applies." t nil)

(autoload 'aya-yank-snippet "auto-yasnippet" "\
Insert current snippet at point.
To save a snippet permanently, create an empty file and call this." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "auto-yasnippet" '("aya-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-yasnippet-autoloads.el ends here
;;; avy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "avy" "avy.el" (0 0 0 0))
;;; Generated autoloads from avy.el

(autoload 'avy-process "avy" "\
Select one of CANDIDATES using `avy-read'.
Use OVERLAY-FN to visualize the decision overlay.
CLEANUP-FN should take no arguments and remove the effects of
multiple OVERLAY-FN invocations.

\(fn CANDIDATES &optional OVERLAY-FN CLEANUP-FN)" nil nil)

(autoload 'avy-goto-char "avy" "\
Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it).

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-char-in-line "avy" "\
Jump to the currently visible CHAR in the current line.

\(fn CHAR)" t nil)

(autoload 'avy-goto-char-2 "avy" "\
Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

\(fn CHAR1 CHAR2 &optional ARG BEG END)" t nil)

(autoload 'avy-goto-char-2-above "avy" "\
Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR1 CHAR2 &optional ARG)" t nil)

(autoload 'avy-goto-char-2-below "avy" "\
Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR1 CHAR2 &optional ARG)" t nil)

(autoload 'avy-isearch "avy" "\
Jump to one of the current isearch candidates." t nil)

(autoload 'avy-goto-word-0 "avy" "\
Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

\(fn ARG &optional BEG END)" t nil)

(autoload 'avy-goto-whitespace-end "avy" "\
Jump to the end of a whitespace sequence.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

\(fn ARG &optional BEG END)" t nil)

(autoload 'avy-goto-word-1 "avy" "\
Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start.

\(fn CHAR &optional ARG BEG END SYMBOL)" t nil)

(autoload 'avy-goto-word-1-above "avy" "\
Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-word-1-below "avy" "\
Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-symbol-1 "avy" "\
Jump to the currently visible CHAR at a symbol start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-symbol-1-above "avy" "\
Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-symbol-1-below "avy" "\
Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-subword-0 "avy" "\
Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched.

\(fn &optional ARG PREDICATE BEG END)" t nil)

(autoload 'avy-goto-subword-1 "avy" "\
Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-word-or-subword-1 "avy" "\
Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'." t nil)

(autoload 'avy-goto-line "avy" "\
Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG.

\(fn &optional ARG)" t nil)

(autoload 'avy-goto-line-above "avy" "\
Goto visible line above the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

\(fn &optional OFFSET BOTTOM-UP)" t nil)

(autoload 'avy-goto-line-below "avy" "\
Goto visible line below the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

\(fn &optional OFFSET BOTTOM-UP)" t nil)

(autoload 'avy-goto-end-of-line "avy" "\
Call `avy-goto-line' and move to the end of the line.

\(fn &optional ARG)" t nil)

(autoload 'avy-copy-line "avy" "\
Copy a selected line above the current line.
ARG lines can be used.

\(fn ARG)" t nil)

(autoload 'avy-move-line "avy" "\
Move a selected line above the current line.
ARG lines can be used.

\(fn ARG)" t nil)

(autoload 'avy-copy-region "avy" "\
Select two lines and copy the text between them to point.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

\(fn ARG)" t nil)

(autoload 'avy-move-region "avy" "\
Select two lines and move the text between them above the current line." t nil)

(autoload 'avy-kill-region "avy" "\
Select two lines and kill the region between them.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

\(fn ARG)" t nil)

(autoload 'avy-kill-ring-save-region "avy" "\
Select two lines and save the region between them to the kill ring.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn ARG)" t nil)

(autoload 'avy-kill-whole-line "avy" "\
Select line and kill the whole selected line.

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\\[universal-argument] 3 \\[avy-kil-whole-line] kill three lines
starting from the selected line.  \\[universal-argument] -3

\\[avy-kill-whole-line] kill three lines backward including the
selected line.

\(fn ARG)" t nil)

(autoload 'avy-kill-ring-save-whole-line "avy" "\
Select line and save the whole selected line as if killed, but don’t kill it.

This command is similar to `avy-kill-whole-line', except that it
saves the line(s) as if killed, but does not kill it(them).

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\(fn ARG)" t nil)

(autoload 'avy-setup-default "avy" "\
Setup the default shortcuts." nil nil)

(autoload 'avy-goto-char-timer "avy" "\
Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

\(fn &optional ARG)" t nil)

(autoload 'avy-transpose-lines-in-region "avy" "\
Transpose lines in the active region." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "avy" '("avy-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; avy-autoloads.el ends here
;;; benchmark-init-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "benchmark-init" "benchmark-init.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from benchmark-init.el

(autoload 'benchmark-init/activate "benchmark-init" "\
Activate benchmark-init and start collecting data." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "benchmark-init" '("benchmark-init/")))

;;;***

;;;### (autoloads nil "benchmark-init-modes" "benchmark-init-modes.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from benchmark-init-modes.el

(autoload 'benchmark-init/show-durations-tabulated "benchmark-init-modes" "\
Show the benchmark results in a sorted table." t nil)

(autoload 'benchmark-init/show-durations-tree "benchmark-init-modes" "\
Show durations in call-tree." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "benchmark-init-modes" '("benchmark-init/")))

;;;***

;;;### (autoloads nil nil ("benchmark-init-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; benchmark-init-autoloads.el ends here
;;; bind-key-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bind-key" "bind-key.el" (0 0 0 0))
;;; Generated autoloads from bind-key.el

(autoload 'bind-key "bind-key" "\
Bind KEY-NAME to COMMAND in KEYMAP (`global-map' if not passed).

KEY-NAME may be a vector, in which case it is passed straight to
`define-key'. Or it may be a string to be interpreted as
spelled-out keystrokes, e.g., \"C-c C-z\". See documentation of
`edmacro-mode' for details.

COMMAND must be an interactive function or lambda form.

KEYMAP, if present, should be a keymap variable or symbol.
For example:

  (bind-key \"M-h\" #'some-interactive-function my-mode-map)

  (bind-key \"M-h\" #'some-interactive-function 'my-mode-map)

If PREDICATE is non-nil, it is a form evaluated to determine when
a key should be bound. It must return non-nil in such cases.
Emacs can evaluate this form at any time that it does redisplay
or operates on menu data structures, so you should write it so it
can safely be called at any time.

\(fn KEY-NAME COMMAND &optional KEYMAP PREDICATE)" nil t)

(autoload 'unbind-key "bind-key" "\
Unbind the given KEY-NAME, within the KEYMAP (if specified).
See `bind-key' for more details.

\(fn KEY-NAME &optional KEYMAP)" nil t)

(autoload 'bind-key* "bind-key" "\
Similar to `bind-key', but overrides any mode-specific bindings.

\(fn KEY-NAME COMMAND &optional PREDICATE)" nil t)

(autoload 'bind-keys "bind-key" "\
Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted).

\(fn &rest ARGS)" nil t)

(autoload 'bind-keys* "bind-key" "\


\(fn &rest ARGS)" nil t)

(autoload 'describe-personal-keybindings "bind-key" "\
Display all the personal keybindings defined by `bind-key'." t nil)

(register-definition-prefixes "bind-key" '("bind-key" "compare-keybindings" "get-binding-description" "override-global-m" "personal-keybindings"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bind-key-autoloads.el ends here
;;; bing-dict-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bing-dict" "bing-dict.el" (0 0 0 0))
;;; Generated autoloads from bing-dict.el

(autoload 'bing-dict-brief "bing-dict" "\
Show the explanation of WORD from Bing in the echo area.

\(fn WORD &optional SYNC-P)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bing-dict" '("bing-dict-")))

;;;***

;;;### (autoloads nil "bing-dict-cache" "bing-dict-cache.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from bing-dict-cache.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bing-dict-cache" '("bing-dict-")))

;;;***

;;;### (autoloads nil nil ("bing-dict-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bing-dict-autoloads.el ends here
;;; buffer-move-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "buffer-move" "buffer-move.el" (0 0 0 0))
;;; Generated autoloads from buffer-move.el

(autoload 'buf-move-up "buffer-move" "\
Swap the current buffer and the buffer above the split.
   If there is no split, ie now window above the current one, an
   error is signaled." t nil)

(autoload 'buf-move-down "buffer-move" "\
Swap the current buffer and the buffer under the split.
   If there is no split, ie now window under the current one, an
   error is signaled." t nil)

(autoload 'buf-move-left "buffer-move" "\
Swap the current buffer and the buffer on the left of the split.
   If there is no split, ie now window on the left of the current
   one, an error is signaled." t nil)

(autoload 'buf-move-right "buffer-move" "\
Swap the current buffer and the buffer on the right of the split.
   If there is no split, ie now window on the right of the current
   one, an error is signaled." t nil)

(autoload 'buf-move "buffer-move" "\
Begin moving the current buffer to different windows.

Use the arrow keys to move in the desired direction.  Pressing
any other key exits this function." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "buffer-move" '("buf")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; buffer-move-autoloads.el ends here
;;; bui-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bui" "bui.el" (0 0 0 0))
;;; Generated autoloads from bui.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui" '("bui-define-")))

;;;***

;;;### (autoloads nil "bui-button" "bui-button.el" (0 0 0 0))
;;; Generated autoloads from bui-button.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-button" '("bui")))

;;;***

;;;### (autoloads nil "bui-core" "bui-core.el" (0 0 0 0))
;;; Generated autoloads from bui-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-core" '("bui-")))

;;;***

;;;### (autoloads nil "bui-entry" "bui-entry.el" (0 0 0 0))
;;; Generated autoloads from bui-entry.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-entry" '("bui-")))

;;;***

;;;### (autoloads nil "bui-history" "bui-history.el" (0 0 0 0))
;;; Generated autoloads from bui-history.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-history" '("bui-history")))

;;;***

;;;### (autoloads nil "bui-info" "bui-info.el" (0 0 0 0))
;;; Generated autoloads from bui-info.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-info" '("bui-info-")))

;;;***

;;;### (autoloads nil "bui-list" "bui-list.el" (0 0 0 0))
;;; Generated autoloads from bui-list.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-list" '("bui-list-")))

;;;***

;;;### (autoloads nil "bui-utils" "bui-utils.el" (0 0 0 0))
;;; Generated autoloads from bui-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-utils" '("bui-")))

;;;***

;;;### (autoloads nil nil ("bui-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bui-autoloads.el ends here
;;; cargo-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cargo" "cargo.el" (0 0 0 0))
;;; Generated autoloads from cargo.el

(autoload 'cargo-minor-mode "cargo" "\
Cargo minor mode. Used to hold keybindings for cargo-mode.

If called interactively, toggle `cargo minor mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{cargo-minor-mode-command-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "cargo" '("cargo-m"))

;;;***

;;;### (autoloads nil "cargo-process" "cargo-process.el" (0 0 0 0))
;;; Generated autoloads from cargo-process.el

(autoload 'cargo-process-bench "cargo-process" "\
Run the Cargo bench command.
With the prefix argument, modify the command's invocation.
Cargo: Run the benchmarks." t nil)

(autoload 'cargo-process-build "cargo-process" "\
Run the Cargo build command.
With the prefix argument, modify the command's invocation.
Cargo: Compile the current project." t nil)

(autoload 'cargo-process-clean "cargo-process" "\
Run the Cargo clean command.
With the prefix argument, modify the command's invocation.
Cargo: Remove the target directory." t nil)

(autoload 'cargo-process-doc "cargo-process" "\
Run the Cargo doc command.
With the prefix argument, modify the command's invocation.
Cargo: Build this project's and its dependencies' documentation." t nil)

(autoload 'cargo-process-doc-open "cargo-process" "\
Run the Cargo doc command with the --open switch.
With the prefix argument, modify the command's invocation.
Cargo: Open this project's documentation." t nil)

(autoload 'cargo-process-new "cargo-process" "\
Run the Cargo new command.
With the prefix argument, modify the command's invocation.
NAME is the name of your application.
If BIN is t then create a binary application, otherwise a library.
Cargo: Create a new cargo project.

\(fn NAME &optional BIN)" t nil)

(autoload 'cargo-process-init "cargo-process" "\
Run the Cargo init command.
With the prefix argument, modify the command's invocation.
DIRECTORY is the directory you want to create a cargo project in.
If BIN is t then create a binary application, otherwise a library.
Cargo: Create a new cargo project in current directory.

DIRECTORY is created if necessary.

\(fn DIRECTORY &optional BIN)" t nil)

(autoload 'cargo-process-run "cargo-process" "\
Run the Cargo run command.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute src/main.rs." t nil)

(autoload 'cargo-process-run-bin "cargo-process" "\
Run the Cargo run command --bin <name>.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute a specific binary

\(fn COMMAND)" t nil)

(autoload 'cargo-process-run-example "cargo-process" "\
Run the Cargo run command --example <name>.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute with --example <name>.

\(fn COMMAND)" t nil)

(autoload 'cargo-process-search "cargo-process" "\
Run the Cargo search command.
With the prefix argument, modify the command's invocation.
SEARCH-TERM is used as the search term for the Cargo registry.
Cargo: Search registry for crates.

\(fn SEARCH-TERM)" t nil)

(autoload 'cargo-process-test "cargo-process" "\
Run the Cargo test command.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests." t nil)

(autoload 'cargo-process-current-test "cargo-process" "\
Run the Cargo test command for the current test.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests." t nil)

(autoload 'cargo-process-current-file-tests "cargo-process" "\
Run the Cargo test command for the current file.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests." t nil)

(autoload 'cargo-process-update "cargo-process" "\
Run the Cargo update command.
With the prefix argument, modify the command's invocation.
Cargo: Update dependencies listed in Cargo.lock." t nil)

(autoload 'cargo-process-fmt "cargo-process" "\
Run the Cargo fmt command.
With the prefix argument, modify the command's invocation.
Requires Cargo Fmt to be installed." t nil)

(autoload 'cargo-process-outdated "cargo-process" "\
Run the Cargo outdated command.
With the prefix argument, modify the command's invocation.
Requires Cargo Outdated to be installed." t nil)

(autoload 'cargo-process-check "cargo-process" "\
Run the Cargo check command.
With the prefix argument, modify the command's invocation.
Cargo: Check compile the current project.
Requires cargo-check to be installed." t nil)

(autoload 'cargo-process-clippy "cargo-process" "\
Run the Cargo clippy command.
With the prefix argument, modify the command's invocation.
Cargo: Clippy compile the current project.
Requires Cargo clippy to be installed." t nil)

(autoload 'cargo-process-add "cargo-process" "\
Run the Cargo add command.
With the prefix argument, modify the command's invocation.
CRATES is the name of the crate to add.
Cargo: This command allows you to add a dependency to a Cargo.toml manifest file.

\(fn CRATE)" t nil)

(autoload 'cargo-process-audit "cargo-process" "\
Run the Cargo audit command.
With the prefix argument, modify the command's invocation.
Cargo: Audit checks the current project's Cargo.lock for security vulnerabilities.
Requires Cargo Audit to be installed." t nil)

(autoload 'cargo-process-rm "cargo-process" "\
Run the Cargo rm command.
With the prefix argument, modify the command's invocation.
CRATE is the name of the crate to remove.
Cargo: Remove a dependency from a Cargo.toml manifest file.

\(fn CRATE)" t nil)

(autoload 'cargo-process-upgrade "cargo-process" "\
Run the Cargo update command.
With the prefix argument, modify the command's invocation.
If ALL is t then update all crates, otherwise specify CRATES.
Cargo: Upgrade dependencies as specified in the local manifest file

\(fn &optional ALL CRATES)" t nil)

(autoload 'cargo-process-repeat "cargo-process" "\
Run the last cargo-process command." t nil)

(register-definition-prefixes "cargo-process" '("cargo-" "manifest-path-argument" "rustc-errno" "set-rust-backtrace"))

;;;***

;;;### (autoloads nil nil ("cargo-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cargo-autoloads.el ends here
;;; ccls-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ccls" "ccls.el" (0 0 0 0))
;;; Generated autoloads from ccls.el

(register-definition-prefixes "ccls" '("ccls-"))

;;;***

;;;### (autoloads nil "ccls-call-hierarchy" "ccls-call-hierarchy.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ccls-call-hierarchy.el

(register-definition-prefixes "ccls-call-hierarchy" '("ccls-call-hierarchy"))

;;;***

;;;### (autoloads nil "ccls-code-lens" "ccls-code-lens.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ccls-code-lens.el

(register-definition-prefixes "ccls-code-lens" '("ccls-"))

;;;***

;;;### (autoloads nil "ccls-inheritance-hierarchy" "ccls-inheritance-hierarchy.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ccls-inheritance-hierarchy.el

(register-definition-prefixes "ccls-inheritance-hierarchy" '("ccls-inheritance-hierarchy"))

;;;***

;;;### (autoloads nil "ccls-member-hierarchy" "ccls-member-hierarchy.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ccls-member-hierarchy.el

(register-definition-prefixes "ccls-member-hierarchy" '("ccls-member-hierarchy"))

;;;***

;;;### (autoloads nil "ccls-semantic-highlight" "ccls-semantic-highlight.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ccls-semantic-highlight.el

(register-definition-prefixes "ccls-semantic-highlight" '("ccls-"))

;;;***

;;;### (autoloads nil "ccls-tree" "ccls-tree.el" (0 0 0 0))
;;; Generated autoloads from ccls-tree.el

(register-definition-prefixes "ccls-tree" '("ccls-tree-"))

;;;***

;;;### (autoloads nil nil ("ccls-common.el" "ccls-pkg.el") (0 0 0
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ccls-autoloads.el ends here
;;; cfrs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cfrs" "cfrs.el" (0 0 0 0))
;;; Generated autoloads from cfrs.el

(autoload 'cfrs-read "cfrs" "\
Read a string using a pos-frame with given PROMPT and INITIAL-INPUT.

\(fn PROMPT &optional INITIAL-INPUT)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cfrs" '("cfrs-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cfrs-autoloads.el ends here
;;; cider-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cider" "cider.el" (0 0 0 0))
;;; Generated autoloads from cider.el

(autoload 'cider-version "cider" "\
Display CIDER's version." t nil)
 (autoload 'cider-start-map "cider" "CIDER jack-in and connect keymap." t 'keymap)

(autoload 'cider-jack-in-clj "cider" "\
Start an nREPL server for the current project and connect to it.
PARAMS is a plist optionally containing :project-dir and :jack-in-cmd.
With the prefix argument, allow editing of the jack in command; with a
double prefix prompt for all these parameters.

\(fn PARAMS)" t nil)

(autoload 'cider-jack-in-cljs "cider" "\
Start an nREPL server for the current project and connect to it.
PARAMS is a plist optionally containing :project-dir, :jack-in-cmd and
:cljs-repl-type (e.g. Node, Figwheel, etc).  With the prefix argument,
allow editing of the jack in command; with a double prefix prompt for all
these parameters.

\(fn PARAMS)" t nil)

(autoload 'cider-jack-in-clj&cljs "cider" "\
Start an nREPL server and connect with clj and cljs REPLs.
PARAMS is a plist optionally containing :project-dir, :jack-in-cmd and
:cljs-repl-type (e.g. Node, Figwheel, etc).  With the prefix argument,
allow for editing of the jack in command; with a double prefix prompt for
all these parameters.  When SOFT-CLJS-START is non-nil, start cljs REPL
only when the ClojureScript dependencies are met.

\(fn &optional PARAMS SOFT-CLJS-START)" t nil)

(autoload 'cider-connect-sibling-clj "cider" "\
Create a Clojure REPL with the same server as OTHER-REPL.
PARAMS is for consistency with other connection commands and is currently
ignored.  OTHER-REPL defaults to `cider-current-repl' and in programs can
also be a server buffer, in which case a new session with a REPL for that
server is created.

\(fn PARAMS &optional OTHER-REPL)" t nil)

(autoload 'cider-connect-sibling-cljs "cider" "\
Create a ClojureScript REPL with the same server as OTHER-REPL.
PARAMS is a plist optionally containing :cljs-repl-type (e.g. Node,
Figwheel, etc).  All other parameters are inferred from the OTHER-REPL.
OTHER-REPL defaults to `cider-current-repl' but in programs can also be a
server buffer, in which case a new session for that server is created.

\(fn PARAMS &optional OTHER-REPL)" t nil)

(autoload 'cider-connect-clj "cider" "\
Initialize a Clojure connection to an nREPL server.
PARAMS is a plist optionally containing :host, :port and :project-dir.  On
prefix argument, prompt for all the parameters.

\(fn &optional PARAMS)" t nil)

(autoload 'cider-connect-cljs "cider" "\
Initialize a ClojureScript connection to an nREPL server.
PARAMS is a plist optionally containing :host, :port, :project-dir and
:cljs-repl-type (e.g. Node, Figwheel, etc).  On prefix, prompt for all the
parameters regardless of their supplied or default values.

\(fn &optional PARAMS)" t nil)

(autoload 'cider-connect-clj&cljs "cider" "\
Initialize a Clojure and ClojureScript connection to an nREPL server.
PARAMS is a plist optionally containing :host, :port, :project-dir and
:cljs-repl-type (e.g. Node, Figwheel, etc).  When SOFT-CLJS-START is
non-nil, don't start if ClojureScript requirements are not met.

\(fn PARAMS &optional SOFT-CLJS-START)" t nil)

(autoload 'cider "cider" "\
Start a connection of any type interactively." t nil)

(defalias 'cider-jack-in #'cider-jack-in-clj)

(defalias 'cider-connect #'cider-connect-clj)

(with-eval-after-load 'clojure-mode (define-key clojure-mode-map (kbd "C-c M-x") #'cider) (define-key clojure-mode-map (kbd "C-c M-j") #'cider-jack-in-clj) (define-key clojure-mode-map (kbd "C-c M-J") #'cider-jack-in-cljs) (define-key clojure-mode-map (kbd "C-c M-c") #'cider-connect-clj) (define-key clojure-mode-map (kbd "C-c M-C") #'cider-connect-cljs) (define-key clojure-mode-map (kbd "C-c C-x") 'cider-start-map) (define-key clojure-mode-map (kbd "C-c C-s") 'sesman-map) (require 'sesman) (sesman-install-menu clojure-mode-map) (add-hook 'clojure-mode-hook (lambda nil (setq-local sesman-system 'CIDER))))

(register-definition-prefixes "cider" '("cider-"))

;;;***

;;;### (autoloads nil "cider-apropos" "cider-apropos.el" (0 0 0 0))
;;; Generated autoloads from cider-apropos.el

(autoload 'cider-apropos "cider-apropos" "\
Show all symbols whose names match QUERY, a regular expression.
QUERY can also be a list of space-separated words (e.g. take while) which
will be converted to a regular expression (like take.+while) automatically
behind the scenes.  The search may be limited to the namespace NS, and may
optionally search doc strings (based on DOCS-P), include private vars
\(based on PRIVATES-P), and be case-sensitive (based on CASE-SENSITIVE-P).

\(fn QUERY &optional NS DOCS-P PRIVATES-P CASE-SENSITIVE-P)" t nil)

(autoload 'cider-apropos-documentation "cider-apropos" "\
Shortcut for (cider-apropos <query> nil t)." t nil)

(autoload 'cider-apropos-select "cider-apropos" "\
Similar to `cider-apropos', but presents the results in a completing read.
Show all symbols whose names match QUERY, a regular expression.
QUERY can also be a list of space-separated words (e.g. take while) which
will be converted to a regular expression (like take.+while) automatically
behind the scenes.  The search may be limited to the namespace NS, and may
optionally search doc strings (based on DOCS-P), include private vars
\(based on PRIVATES-P), and be case-sensitive (based on CASE-SENSITIVE-P).

\(fn QUERY &optional NS DOCS-P PRIVATES-P CASE-SENSITIVE-P)" t nil)

(autoload 'cider-apropos-documentation-select "cider-apropos" "\
Shortcut for (cider-apropos-select <query> nil t)." t nil)

(register-definition-prefixes "cider-apropos" '("apropos-special-form" "cider-"))

;;;***

;;;### (autoloads nil "cider-browse-ns" "cider-browse-ns.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from cider-browse-ns.el

(autoload 'cider-browse-ns "cider-browse-ns" "\
List all NAMESPACE's vars in BUFFER.

\(fn NAMESPACE)" t nil)

(autoload 'cider-browse-ns-all "cider-browse-ns" "\
List all loaded namespaces in BUFFER." t nil)

(register-definition-prefixes "cider-browse-ns" '("cider-browse-ns-"))

;;;***

;;;### (autoloads nil "cider-browse-spec" "cider-browse-spec.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from cider-browse-spec.el

(autoload 'cider-browse-spec "cider-browse-spec" "\
Browse SPEC definition.

\(fn SPEC)" t nil)

(autoload 'cider-browse-spec-all "cider-browse-spec" "\
Open list of specs in a popup buffer.

With a prefix argument ARG, prompts for a regexp to filter specs.
No filter applied if the regexp is the empty string.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "cider-browse-spec" '("cider-"))

;;;***

;;;### (autoloads nil "cider-cheatsheet" "cider-cheatsheet.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from cider-cheatsheet.el

(autoload 'cider-cheatsheet "cider-cheatsheet" "\
Navigate `cider-cheatsheet-hierarchy' with `completing-read'.

When you make it to a Clojure var its doc buffer gets displayed." t nil)

(register-definition-prefixes "cider-cheatsheet" '("cider-cheatsheet-"))

;;;***

;;;### (autoloads nil "cider-classpath" "cider-classpath.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from cider-classpath.el

(autoload 'cider-classpath "cider-classpath" "\
List all classpath entries." t nil)

(autoload 'cider-open-classpath-entry "cider-classpath" "\
Open a classpath entry." t nil)

(register-definition-prefixes "cider-classpath" '("cider-classpath-"))

;;;***

;;;### (autoloads nil "cider-client" "cider-client.el" (0 0 0 0))
;;; Generated autoloads from cider-client.el

(register-definition-prefixes "cider-client" '("cider-"))

;;;***

;;;### (autoloads nil "cider-clojuredocs" "cider-clojuredocs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from cider-clojuredocs.el

(autoload 'cider-clojuredocs-web "cider-clojuredocs" "\
Open ClojureDocs documentation in the default web browser.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates.

\(fn &optional ARG)" t nil)

(autoload 'cider-clojuredocs-refresh-cache "cider-clojuredocs" "\
Refresh the ClojureDocs cache." t nil)

(autoload 'cider-clojuredocs "cider-clojuredocs" "\
Open ClojureDocs documentation in a popup buffer.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "cider-clojuredocs" '("cider-"))

;;;***

;;;### (autoloads nil "cider-common" "cider-common.el" (0 0 0 0))
;;; Generated autoloads from cider-common.el

(register-definition-prefixes "cider-common" '("cider-"))

;;;***

;;;### (autoloads nil "cider-completion" "cider-completion.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from cider-completion.el

(register-definition-prefixes "cider-completion" '("cider-"))

;;;***

;;;### (autoloads nil "cider-connection" "cider-connection.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from cider-connection.el

(register-definition-prefixes "cider-connection" '("cider-"))

;;;***

;;;### (autoloads nil "cider-debug" "cider-debug.el" (0 0 0 0))
;;; Generated autoloads from cider-debug.el

(autoload 'cider-debug-defun-at-point "cider-debug" "\
Instrument the \"top-level\" expression at point.
If it is a defn, dispatch the instrumented definition.  Otherwise,
immediately evaluate the instrumented expression.

While debugged code is being evaluated, the user is taken through the
source code and displayed the value of various expressions.  At each step,
a number of keys will be prompted to the user." t nil)

(register-definition-prefixes "cider-debug" '("cider-"))

;;;***

;;;### (autoloads nil "cider-doc" "cider-doc.el" (0 0 0 0))
;;; Generated autoloads from cider-doc.el

(register-definition-prefixes "cider-doc" '("cider-"))

;;;***

;;;### (autoloads nil "cider-eldoc" "cider-eldoc.el" (0 0 0 0))
;;; Generated autoloads from cider-eldoc.el

(register-definition-prefixes "cider-eldoc" '("cider-"))

;;;***

;;;### (autoloads nil "cider-eval" "cider-eval.el" (0 0 0 0))
;;; Generated autoloads from cider-eval.el

(register-definition-prefixes "cider-eval" '("cider-"))

;;;***

;;;### (autoloads nil "cider-find" "cider-find.el" (0 0 0 0))
;;; Generated autoloads from cider-find.el

(autoload 'cider-find-var "cider-find" "\
Find definition for VAR at LINE.
Prompt according to prefix ARG and `cider-prompt-for-symbol'.
A single or double prefix argument inverts the meaning of
`cider-prompt-for-symbol'.  A prefix of `-` or a double prefix argument causes
the results to be displayed in a different window.  The default value is
thing at point.

\(fn &optional ARG VAR LINE)" t nil)

(autoload 'cider-find-dwim-at-mouse "cider-find" "\
Find and display variable or resource at mouse EVENT.

\(fn EVENT)" t nil)

(autoload 'cider-find-dwim "cider-find" "\
Find and display the SYMBOL-FILE at point.
SYMBOL-FILE could be a var or a resource.  If thing at point is empty then
show dired on project.  If var is not found, try to jump to resource of the
same name.  When called interactively, a prompt is given according to the
variable `cider-prompt-for-symbol'.  A single or double prefix argument
inverts the meaning.  A prefix of `-' or a double prefix argument causes
the results to be displayed in a different window.  A default value of thing
at point is given when prompted.

\(fn SYMBOL-FILE)" t nil)

(autoload 'cider-find-resource "cider-find" "\
Find the resource at PATH.
Prompt for input as indicated by the variable `cider-prompt-for-symbol'.
A single or double prefix argument inverts the meaning of
`cider-prompt-for-symbol'.  A prefix argument of `-` or a double prefix
argument causes the results to be displayed in other window.  The default
value is thing at point.

\(fn PATH)" t nil)

(autoload 'cider-find-ns "cider-find" "\
Find the file containing NS.
A prefix ARG of `-` or a double prefix argument causes
the results to be displayed in a different window.

\(fn &optional ARG NS)" t nil)

(autoload 'cider-find-keyword "cider-find" "\
Find the namespace of the keyword at point and its first occurrence there.

For instance - if the keyword at point is \":cider.demo/keyword\", this command
would find the namespace \"cider.demo\" and afterwards find the first mention
of \"::keyword\" there.

Prompt according to prefix ARG and `cider-prompt-for-symbol'.
A single or double prefix argument inverts the meaning of
`cider-prompt-for-symbol'.  A prefix of `-` or a double prefix argument causes
the results to be displayed in a different window.  The default value is
thing at point.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "cider-find" '("cider-"))

;;;***

;;;### (autoloads nil "cider-format" "cider-format.el" (0 0 0 0))
;;; Generated autoloads from cider-format.el

(autoload 'cider-format-region "cider-format" "\
Format the Clojure code in the current region.
START and END represent the region's boundaries.

\(fn START END)" t nil)

(autoload 'cider-format-defun "cider-format" "\
Format the code in the current defun." t nil)

(autoload 'cider-format-buffer "cider-format" "\
Format the Clojure code in the current buffer." t nil)

(autoload 'cider-format-edn-buffer "cider-format" "\
Format the EDN data in the current buffer." t nil)

(autoload 'cider-format-edn-region "cider-format" "\
Format the EDN data in the current region.
START and END represent the region's boundaries.

\(fn START END)" t nil)

(autoload 'cider-format-edn-last-sexp "cider-format" "\
Format the EDN data of the last sexp." t nil)

(register-definition-prefixes "cider-format" '("cider--format-"))

;;;***

;;;### (autoloads nil "cider-inspector" "cider-inspector.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from cider-inspector.el

(autoload 'cider-inspect-last-sexp "cider-inspector" "\
Inspect the result of the the expression preceding point." t nil)

(autoload 'cider-inspect-defun-at-point "cider-inspector" "\
Inspect the result of the \"top-level\" expression at point." t nil)

(autoload 'cider-inspect-last-result "cider-inspector" "\
Inspect the most recent eval result." t nil)

(autoload 'cider-inspect "cider-inspector" "\
Inspect the result of the preceding sexp.

With a prefix argument ARG it inspects the result of the \"top-level\" form.
With a second prefix argument it prompts for an expression to eval and inspect.

\(fn &optional ARG)" t nil)

(autoload 'cider-inspect-expr "cider-inspector" "\
Evaluate EXPR in NS and inspect its value.
Interactively, EXPR is read from the minibuffer, and NS the
current buffer's namespace.

\(fn EXPR NS)" t nil)

(register-definition-prefixes "cider-inspector" '("cider-"))

;;;***

;;;### (autoloads nil "cider-macroexpansion" "cider-macroexpansion.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from cider-macroexpansion.el

(autoload 'cider-macroexpand-1 "cider-macroexpansion" "\
Invoke \\=`macroexpand-1\\=` on the expression preceding point.
If invoked with a PREFIX argument, use \\=`macroexpand\\=` instead of
\\=`macroexpand-1\\=`.

\(fn &optional PREFIX)" t nil)

(autoload 'cider-macroexpand-all "cider-macroexpansion" "\
Invoke \\=`macroexpand-all\\=` on the expression preceding point." t nil)

(register-definition-prefixes "cider-macroexpansion" '("cider-"))

;;;***

;;;### (autoloads nil "cider-mode" "cider-mode.el" (0 0 0 0))
;;; Generated autoloads from cider-mode.el

(defvar cider-mode-line '(:eval (format " cider[%s]" (cider--modeline-info))) "\
Mode line lighter for cider mode.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for details
about mode line templates.

Customize this variable to change how cider mode displays its status in the
mode line.  The default value displays the current connection.  Set this
variable to nil to disable the mode line entirely.")

(custom-autoload 'cider-mode-line "cider-mode" t)

(with-eval-after-load 'clojure-mode (easy-menu-define cider-clojure-mode-menu-open clojure-mode-map "Menu for Clojure mode.\n  This is displayed in `clojure-mode' buffers, if `cider-mode' is not active." `("CIDER" :visible (not cider-mode) ["Start a Clojure REPL" cider-jack-in-clj :help "Starts an nREPL server and connects a Clojure REPL to it."] ["Connect to a Clojure REPL" cider-connect-clj :help "Connects to a REPL that's already running."] ["Start a ClojureScript REPL" cider-jack-in-cljs :help "Starts an nREPL server and connects a ClojureScript REPL to it."] ["Connect to a ClojureScript REPL" cider-connect-cljs :help "Connects to a ClojureScript REPL that's already running."] ["Start a Clojure REPL, and a ClojureScript REPL" cider-jack-in-clj&cljs :help "Starts an nREPL server, connects a Clojure REPL to it, and then a ClojureScript REPL."] "--" ["View manual online" cider-view-manual])))

(autoload 'cider-mode "cider-mode" "\
Minor mode for REPL interaction from a Clojure buffer.

If called interactively, toggle `Cider mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{cider-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "cider-mode" '("cider-"))

;;;***

;;;### (autoloads nil "cider-ns" "cider-ns.el" (0 0 0 0))
;;; Generated autoloads from cider-ns.el

(autoload 'cider-ns-reload "cider-ns" "\
Send a (require 'ns :reload) to the REPL.

With an argument PROMPT, it prompts for a namespace name.  This is the
Clojure out of the box reloading experience and does not rely on
org.clojure/tools.namespace.  See Commentary of this file for a longer list
of differences.  From the Clojure doc: \":reload forces loading of all the
identified libs even if they are already loaded\".

\(fn &optional PROMPT)" t nil)

(autoload 'cider-ns-reload-all "cider-ns" "\
Send a (require 'ns :reload-all) to the REPL.

With an argument PROMPT, it prompts for a namespace name.  This is the
Clojure out of the box reloading experience and does not rely on
org.clojure/tools.namespace.  See Commentary of this file for a longer list
of differences.  From the Clojure doc: \":reload-all implies :reload and
also forces loading of all libs that the identified libs directly or
indirectly load via require\".

\(fn &optional PROMPT)" t nil)

(autoload 'cider-ns-refresh "cider-ns" "\
Reload modified and unloaded namespaces on the classpath.

With a single prefix argument, or if MODE is `refresh-all', reload all
namespaces on the classpath unconditionally.

With a double prefix argument, or if MODE is `clear', clear the state of
the namespace tracker before reloading.  This is useful for recovering from
some classes of error (for example, those caused by circular dependencies)
that a normal reload would not otherwise recover from.  The trade-off of
clearing is that stale code from any deleted files may not be completely
unloaded.

With a negative prefix argument, or if MODE is `inhibit-fns', prevent any
refresh functions (defined in `cider-ns-refresh-before-fn' and
`cider-ns-refresh-after-fn') from being invoked.

\(fn &optional MODE)" t nil)

(register-definition-prefixes "cider-ns" '("cider-ns-"))

;;;***

;;;### (autoloads nil "cider-overlays" "cider-overlays.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from cider-overlays.el

(register-definition-prefixes "cider-overlays" '("cider-"))

;;;***

;;;### (autoloads nil "cider-popup" "cider-popup.el" (0 0 0 0))
;;; Generated autoloads from cider-popup.el

(register-definition-prefixes "cider-popup" '("cider-"))

;;;***

;;;### (autoloads nil "cider-profile" "cider-profile.el" (0 0 0 0))
;;; Generated autoloads from cider-profile.el

(autoload 'cider-profile-samples "cider-profile" "\
Displays current max-sample-count.
If optional QUERY is specified, set max-sample-count and display new value.

\(fn &optional QUERY)" t nil)

(autoload 'cider-profile-var-profiled-p "cider-profile" "\
Displays the profiling status of var under point.
Prompts for var if none under point or QUERY is present.

\(fn QUERY)" t nil)

(autoload 'cider-profile-ns-toggle "cider-profile" "\
Toggle profiling for the ns associated with optional QUERY.

If optional argument QUERY is non-nil, prompt for ns.  Otherwise use
current ns.

\(fn &optional QUERY)" t nil)

(autoload 'cider-profile-toggle "cider-profile" "\
Toggle profiling for the given QUERY.
Defaults to the symbol at point.
With prefix arg or no symbol at point, prompts for a var.

\(fn QUERY)" t nil)

(autoload 'cider-profile-summary "cider-profile" "\
Display a summary of currently collected profile data." t nil)

(autoload 'cider-profile-var-summary "cider-profile" "\
Display profile data for var under point QUERY.
Defaults to the symbol at point.  With prefix arg or no symbol at point,
prompts for a var.

\(fn QUERY)" t nil)

(autoload 'cider-profile-clear "cider-profile" "\
Clear any collected profile data." t nil)

(register-definition-prefixes "cider-profile" '("cider-profile-"))

;;;***

;;;### (autoloads nil "cider-repl" "cider-repl.el" (0 0 0 0))
;;; Generated autoloads from cider-repl.el

(register-definition-prefixes "cider-repl" '("cider-"))

;;;***

;;;### (autoloads nil "cider-repl-history" "cider-repl-history.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from cider-repl-history.el

(autoload 'cider-repl-history "cider-repl-history" "\
Display items in the CIDER command history in another buffer." t nil)

(register-definition-prefixes "cider-repl-history" '("cider-repl-history-"))

;;;***

;;;### (autoloads nil "cider-resolve" "cider-resolve.el" (0 0 0 0))
;;; Generated autoloads from cider-resolve.el

(register-definition-prefixes "cider-resolve" '("cider-resolve-"))

;;;***

;;;### (autoloads nil "cider-scratch" "cider-scratch.el" (0 0 0 0))
;;; Generated autoloads from cider-scratch.el

(autoload 'cider-scratch "cider-scratch" "\
Go to the scratch buffer named `cider-scratch-buffer-name'." t nil)

(register-definition-prefixes "cider-scratch" '("cider-"))

;;;***

;;;### (autoloads nil "cider-selector" "cider-selector.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from cider-selector.el

(autoload 'cider-selector "cider-selector" "\
Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer.  The `?' character describes the
available methods.  OTHER-WINDOW provides an optional target.
See `def-cider-selector-method' for defining new methods.

\(fn &optional OTHER-WINDOW)" t nil)

(register-definition-prefixes "cider-selector" '("??" "?c" "?d" "?e" "?m" "?p" "?q" "?r" "?s" "?x" "cider-selector-" "def-cider-selector-method"))

;;;***

;;;### (autoloads nil "cider-stacktrace" "cider-stacktrace.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from cider-stacktrace.el

(register-definition-prefixes "cider-stacktrace" '("cider-"))

;;;***

;;;### (autoloads nil "cider-test" "cider-test.el" (0 0 0 0))
;;; Generated autoloads from cider-test.el

(defvar cider-auto-test-mode nil "\
Non-nil if Cider-Auto-Test mode is enabled.
See the `cider-auto-test-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `cider-auto-test-mode'.")

(custom-autoload 'cider-auto-test-mode "cider-test" nil)

(autoload 'cider-auto-test-mode "cider-test" "\
Toggle automatic testing of Clojure files.

If called interactively, toggle `Cider-Auto-Test mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When enabled this reruns tests every time a Clojure file is loaded.
Only runs tests corresponding to the loaded file's namespace and does
nothing if no tests are defined or if the file failed to load.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "cider-test" '("cider-"))

;;;***

;;;### (autoloads nil "cider-tracing" "cider-tracing.el" (0 0 0 0))
;;; Generated autoloads from cider-tracing.el

(autoload 'cider-toggle-trace-var "cider-tracing" "\
Toggle var tracing.
Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates.

\(fn ARG)" t nil)

(autoload 'cider-toggle-trace-ns "cider-tracing" "\
Toggle ns tracing.
Defaults to the current ns.  With prefix arg QUERY, prompts for a ns.

\(fn QUERY)" t nil)

(register-definition-prefixes "cider-tracing" '("cider-"))

;;;***

;;;### (autoloads nil "cider-util" "cider-util.el" (0 0 0 0))
;;; Generated autoloads from cider-util.el

(autoload 'cider-view-manual "cider-util" "\
View the manual in your default browser." t nil)

(register-definition-prefixes "cider-util" '("cider-"))

;;;***

;;;### (autoloads nil "cider-xref" "cider-xref.el" (0 0 0 0))
;;; Generated autoloads from cider-xref.el

(autoload 'cider-xref-fn-refs "cider-xref" "\
Show all functions that reference the var matching NS and SYMBOL.

\(fn &optional NS SYMBOL)" t nil)

(autoload 'cider-xref-fn-deps "cider-xref" "\
Show all functions referenced by the var matching NS and SYMBOL.

\(fn &optional NS SYMBOL)" t nil)

(autoload 'cider-xref-fn-refs-select "cider-xref" "\
Displays the references for NS and SYMBOL using completing read.

\(fn &optional NS SYMBOL)" t nil)

(autoload 'cider-xref-fn-deps-select "cider-xref" "\
Displays the function dependencies for  NS and SYMBOL using completing read.

\(fn &optional NS SYMBOL)" t nil)

(register-definition-prefixes "cider-xref" '("cider-"))

;;;***

;;;### (autoloads nil "nrepl-client" "nrepl-client.el" (0 0 0 0))
;;; Generated autoloads from nrepl-client.el

(register-definition-prefixes "nrepl-client" '("cider-enlighten-mode" "emacs-bug-46284/when-27.1-windows-nt" "nrepl-"))

;;;***

;;;### (autoloads nil "nrepl-dict" "nrepl-dict.el" (0 0 0 0))
;;; Generated autoloads from nrepl-dict.el

(register-definition-prefixes "nrepl-dict" '("nrepl-"))

;;;***

;;;### (autoloads nil nil ("cider-compat.el" "cider-pkg.el") (0 0
;;;;;;  0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cider-autoloads.el ends here
;;; clojure-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "clojure-mode" "clojure-mode.el" (0 0 0 0))
;;; Generated autoloads from clojure-mode.el

(autoload 'clojure-mode "clojure-mode" "\
Major mode for editing Clojure code.

\\{clojure-mode-map}

\(fn)" t nil)

(autoload 'clojure-unwind "clojure-mode" "\
Unwind thread at point or above point by N levels.
With universal argument \\[universal-argument], fully unwind thread.

\(fn &optional N)" t nil)

(autoload 'clojure-unwind-all "clojure-mode" "\
Fully unwind thread at point or above point." t nil)

(autoload 'clojure-thread "clojure-mode" "\
Thread by one more level an existing threading macro." t nil)

(autoload 'clojure-thread-first-all "clojure-mode" "\
Fully thread the form at point using ->.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `clojure-thread-all-but-last'.

\(fn BUT-LAST)" t nil)

(autoload 'clojure-thread-last-all "clojure-mode" "\
Fully thread the form at point using ->>.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `clojure-thread-all-but-last'.

\(fn BUT-LAST)" t nil)

(autoload 'clojure-cycle-privacy "clojure-mode" "\
Make public the current private def, or vice-versa.
See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-privacy" t nil)

(autoload 'clojure-convert-collection-to-list "clojure-mode" "\
Convert collection at (point) to list." t nil)

(autoload 'clojure-convert-collection-to-quoted-list "clojure-mode" "\
Convert collection at (point) to quoted list." t nil)

(autoload 'clojure-convert-collection-to-map "clojure-mode" "\
Convert collection at (point) to map." t nil)

(autoload 'clojure-convert-collection-to-vector "clojure-mode" "\
Convert collection at (point) to vector." t nil)

(autoload 'clojure-convert-collection-to-set "clojure-mode" "\
Convert collection at (point) to set." t nil)

(autoload 'clojure-cycle-if "clojure-mode" "\
Change a surrounding if to if-not, or vice-versa.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-if" t nil)

(autoload 'clojure-cycle-when "clojure-mode" "\
Change a surrounding when to when-not, or vice-versa." t nil)

(autoload 'clojure-let-backward-slurp-sexp "clojure-mode" "\
Slurp the s-expression before the let form into the let form.
With a numeric prefix argument slurp the previous N s-expressions
into the let form.

\(fn &optional N)" t nil)

(autoload 'clojure-let-forward-slurp-sexp "clojure-mode" "\
Slurp the next s-expression after the let form into the let form.
With a numeric prefix argument slurp the next N s-expressions
into the let form.

\(fn &optional N)" t nil)

(autoload 'clojure-introduce-let "clojure-mode" "\
Create a let form, binding the form at point.
With a numeric prefix argument the let is introduced N lists up.

\(fn &optional N)" t nil)

(autoload 'clojure-move-to-let "clojure-mode" "\
Move the form at point to a binding in the nearest let." t nil)

(autoload 'clojure-rename-ns-alias "clojure-mode" "\
Rename a namespace alias." t nil)

(autoload 'clojure-add-arity "clojure-mode" "\
Add an arity to a function." t nil)

(autoload 'clojurescript-mode "clojure-mode" "\
Major mode for editing ClojureScript code.

\\{clojurescript-mode-map}

\(fn)" t nil)

(autoload 'clojurec-mode "clojure-mode" "\
Major mode for editing ClojureC code.

\\{clojurec-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojurec-mode))

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))

(add-to-list 'auto-mode-alist '("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode))

(register-definition-prefixes "clojure-mode" '("add-custom-clojure-indents" "clojure" "define-clojure-indent" "put-clojure-indent"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; clojure-mode-autoloads.el ends here
;;; cmake-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cmake-mode" "cmake-mode.el" (0 0 0 0))
;;; Generated autoloads from cmake-mode.el

(autoload 'cmake-mode "cmake-mode" "\
Major mode for editing CMake source files.

\(fn)" t nil)

(autoload 'cmake-command-run "cmake-mode" "\
Runs the command cmake with the arguments specified.  The
optional argument topic will be appended to the argument list.

\(fn TYPE &optional TOPIC BUFFER)" t nil)

(autoload 'cmake-command-run-help "cmake-mode" "\
`cmake-command-run' but rendered in `rst-mode'.

\(fn TYPE &optional TOPIC BUFFER)" t nil)

(autoload 'cmake-help-list-commands "cmake-mode" "\
Prints out a list of the cmake commands." t nil)

(autoload 'cmake-help-command "cmake-mode" "\
Prints out the help message for the command the cursor is on." t nil)

(autoload 'cmake-help-module "cmake-mode" "\
Prints out the help message for the module the cursor is on." t nil)

(autoload 'cmake-help-variable "cmake-mode" "\
Prints out the help message for the variable the cursor is on." t nil)

(autoload 'cmake-help-property "cmake-mode" "\
Prints out the help message for the property the cursor is on." t nil)

(autoload 'cmake-help "cmake-mode" "\
Queries for any of the four available help topics and prints out the appropriate page." t nil)

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))

(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cmake-mode" '("cmake-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cmake-mode-autoloads.el ends here
;;; command-log-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "command-log-mode" "command-log-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from command-log-mode.el

(autoload 'command-log-mode "command-log-mode" "\
Toggle keyboard command logging.

If called interactively, enable Command-Log mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'clm/toggle-command-log-buffer "command-log-mode" "\
Toggle the command log showing or not.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "command-log-mode" '("clm/" "command-log-mode-" "global-command-log-mode")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; command-log-mode-autoloads.el ends here
;;; company-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company" "company.el" (0 0 0 0))
;;; Generated autoloads from company.el

(autoload 'company-mode "company" "\
\"complete anything\"; is an in-buffer completion framework.
Completion starts automatically, depending on the values
`company-idle-delay' and `company-minimum-prefix-length'.

If called interactively, toggle `Company mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Completion can be controlled with the commands:
`company-complete-common', `company-complete-selection', `company-complete',
`company-select-next', `company-select-previous'.  If these commands are
called before `company-idle-delay', completion will also start.

Completions can be searched with `company-search-candidates' or
`company-filter-candidates'.  These can be used while completion is
inactive, as well.

The completion data is retrieved using `company-backends' and displayed
using `company-frontends'.  If you want to start a specific backend, call
it interactively or use `company-begin-backend'.

By default, the completions list is sorted alphabetically, unless the
backend chooses otherwise, or `company-transformers' changes it later.

regular keymap (`company-mode-map'):

\\{company-mode-map}
keymap during active completions (`company-active-map'):

\\{company-active-map}

\(fn &optional ARG)" t nil)

(put 'global-company-mode 'globalized-minor-mode t)

(defvar global-company-mode nil "\
Non-nil if Global Company mode is enabled.
See the `global-company-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-company-mode'.")

(custom-autoload 'global-company-mode "company" nil)

(autoload 'global-company-mode "company" "\
Toggle Company mode in all buffers.
With prefix ARG, enable Global Company mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG is
omitted or nil.

Company mode is enabled in all buffers where `company-mode-on' would
do it.

See `company-mode' for more information on Company mode.

\(fn &optional ARG)" t nil)

(autoload 'company-manual-begin "company" nil t nil)

(autoload 'company-complete "company" "\
Insert the common part of all candidates or the current selection.
The first time this is called, the common part is inserted, the second
time, or when the selection has been changed, the selected candidate is
inserted." t nil)

(register-definition-prefixes "company" '("company-"))

;;;***

;;;### (autoloads nil "company-abbrev" "company-abbrev.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from company-abbrev.el

(autoload 'company-abbrev "company-abbrev" "\
`company-mode' completion backend for abbrev.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-abbrev" '("company-abbrev-insert"))

;;;***

;;;### (autoloads nil "company-bbdb" "company-bbdb.el" (0 0 0 0))
;;; Generated autoloads from company-bbdb.el

(autoload 'company-bbdb "company-bbdb" "\
`company-mode' completion backend for BBDB.

\(fn COMMAND &optional ARG &rest IGNORE)" t nil)

(register-definition-prefixes "company-bbdb" '("company-bbdb-"))

;;;***

;;;### (autoloads nil "company-capf" "company-capf.el" (0 0 0 0))
;;; Generated autoloads from company-capf.el

(register-definition-prefixes "company-capf" '("company-"))

;;;***

;;;### (autoloads nil "company-clang" "company-clang.el" (0 0 0 0))
;;; Generated autoloads from company-clang.el

(register-definition-prefixes "company-clang" '("company-clang"))

;;;***

;;;### (autoloads nil "company-cmake" "company-cmake.el" (0 0 0 0))
;;; Generated autoloads from company-cmake.el

(register-definition-prefixes "company-cmake" '("company-cmake"))

;;;***

;;;### (autoloads nil "company-css" "company-css.el" (0 0 0 0))
;;; Generated autoloads from company-css.el

(autoload 'company-css "company-css" "\
`company-mode' completion backend for `css-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-css" '("company-css-"))

;;;***

;;;### (autoloads nil "company-dabbrev" "company-dabbrev.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from company-dabbrev.el

(autoload 'company-dabbrev "company-dabbrev" "\
dabbrev-like `company-mode' completion backend.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-dabbrev" '("company-dabbrev-"))

;;;***

;;;### (autoloads nil "company-dabbrev-code" "company-dabbrev-code.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-dabbrev-code.el

(autoload 'company-dabbrev-code "company-dabbrev-code" "\
dabbrev-like `company-mode' backend for code.
The backend looks for all symbols in the current buffer that aren't in
comments or strings.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-dabbrev-code" '("company-dabbrev-code-"))

;;;***

;;;### (autoloads nil "company-elisp" "company-elisp.el" (0 0 0 0))
;;; Generated autoloads from company-elisp.el

(autoload 'company-elisp "company-elisp" "\
`company-mode' completion backend for Emacs Lisp.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-elisp" '("company-elisp-"))

;;;***

;;;### (autoloads nil "company-etags" "company-etags.el" (0 0 0 0))
;;; Generated autoloads from company-etags.el

(autoload 'company-etags "company-etags" "\
`company-mode' completion backend for etags.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-etags" '("company-etags-"))

;;;***

;;;### (autoloads nil "company-files" "company-files.el" (0 0 0 0))
;;; Generated autoloads from company-files.el

(autoload 'company-files "company-files" "\
`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-files" '("company-file"))

;;;***

;;;### (autoloads nil "company-gtags" "company-gtags.el" (0 0 0 0))
;;; Generated autoloads from company-gtags.el

(autoload 'company-gtags "company-gtags" "\
`company-mode' completion backend for GNU Global.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-gtags" '("company-gtags-"))

;;;***

;;;### (autoloads nil "company-ispell" "company-ispell.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from company-ispell.el

(autoload 'company-ispell "company-ispell" "\
`company-mode' completion backend using Ispell.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-ispell" '("company-ispell-"))

;;;***

;;;### (autoloads nil "company-keywords" "company-keywords.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from company-keywords.el

(autoload 'company-keywords "company-keywords" "\
`company-mode' backend for programming language keywords.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-keywords" '("company-keywords-"))

;;;***

;;;### (autoloads nil "company-nxml" "company-nxml.el" (0 0 0 0))
;;; Generated autoloads from company-nxml.el

(autoload 'company-nxml "company-nxml" "\
`company-mode' completion backend for `nxml-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-nxml" '("company-nxml-"))

;;;***

;;;### (autoloads nil "company-oddmuse" "company-oddmuse.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from company-oddmuse.el

(autoload 'company-oddmuse "company-oddmuse" "\
`company-mode' completion backend for `oddmuse-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-oddmuse" '("company-oddmuse-"))

;;;***

;;;### (autoloads nil "company-semantic" "company-semantic.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from company-semantic.el

(autoload 'company-semantic "company-semantic" "\
`company-mode' completion backend using CEDET Semantic.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-semantic" '("company-semantic-"))

;;;***

;;;### (autoloads nil "company-template" "company-template.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from company-template.el

(register-definition-prefixes "company-template" '("company-template-"))

;;;***

;;;### (autoloads nil "company-tempo" "company-tempo.el" (0 0 0 0))
;;; Generated autoloads from company-tempo.el

(autoload 'company-tempo "company-tempo" "\
`company-mode' completion backend for tempo.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-tempo" '("company-tempo-"))

;;;***

;;;### (autoloads nil "company-tng" "company-tng.el" (0 0 0 0))
;;; Generated autoloads from company-tng.el

(autoload 'company-tng-frontend "company-tng" "\
When the user changes the selection at least once, this
frontend will display the candidate in the buffer as if it's
already there and any key outside of `company-active-map' will
confirm the selection and finish the completion.

\(fn COMMAND)" nil nil)

(define-obsolete-function-alias 'company-tng-configure-default 'company-tng-mode "0.9.14" "\
Applies the default configuration to enable company-tng.")

(defvar company-tng-mode nil "\
Non-nil if Company-Tng mode is enabled.
See the `company-tng-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-tng-mode'.")

(custom-autoload 'company-tng-mode "company-tng" nil)

(autoload 'company-tng-mode "company-tng" "\
This minor mode enables `company-tng-frontend'.

If called interactively, toggle `Company-Tng mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "company-tng" '("company-tng-"))

;;;***

;;;### (autoloads nil "company-yasnippet" "company-yasnippet.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-yasnippet.el

(autoload 'company-yasnippet "company-yasnippet" "\
`company-mode' backend for `yasnippet'.

This backend should be used with care, because as long as there are
snippets defined for the current major mode, this backend will always
shadow backends that come after it.  Recommended usages:

* In a buffer-local value of `company-backends', grouped with a backend or
  several that provide actual text completions.

  (add-hook \\='js-mode-hook
            (lambda ()
              (set (make-local-variable \\='company-backends)
                   \\='((company-dabbrev-code company-yasnippet)))))

* After keyword `:with', grouped with other backends.

  (push \\='(company-semantic :with company-yasnippet) company-backends)

* Not in `company-backends', just bound to a key.

  (global-set-key (kbd \"C-c y\") \\='company-yasnippet)

\(fn COMMAND &optional ARG &rest IGNORE)" t nil)

(register-definition-prefixes "company-yasnippet" '("company-yasnippet-"))

;;;***

;;;### (autoloads nil nil ("company-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-autoloads.el ends here
;;; company-box-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-box" "company-box.el" (0 0 0 0))
;;; Generated autoloads from company-box.el

(autoload 'company-box-mode "company-box" "\
Company-box minor mode.

If called interactively, toggle `Company-Box mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "company-box" '("company-box-"))

;;;***

;;;### (autoloads nil "company-box-doc" "company-box-doc.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from company-box-doc.el

(register-definition-prefixes "company-box-doc" '("company-box-"))

;;;***

;;;### (autoloads nil "company-box-icons" "company-box-icons.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-box-icons.el

(register-definition-prefixes "company-box-icons" '("company-box-icons-"))

;;;***

;;;### (autoloads nil nil ("company-box-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-box-autoloads.el ends here
;;; company-prescient-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-prescient" "company-prescient.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-prescient.el

(defvar company-prescient-mode nil "\
Non-nil if Company-Prescient mode is enabled.
See the `company-prescient-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-prescient-mode'.")

(custom-autoload 'company-prescient-mode "company-prescient" nil)

(autoload 'company-prescient-mode "company-prescient" "\
Minor mode to use prescient.el in Company completions.

If called interactively, enable Company-Prescient mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-prescient" '("company-prescient-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-prescient-autoloads.el ends here
;;; company-quickhelp-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-quickhelp" "company-quickhelp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-quickhelp.el

(autoload 'company-quickhelp-local-mode "company-quickhelp" "\
Provides documentation popups for `company-mode' using `pos-tip'.

If called interactively, toggle `Company-Quickhelp-Local mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'company-quickhelp-mode 'globalized-minor-mode t)

(defvar company-quickhelp-mode nil "\
Non-nil if Company-Quickhelp mode is enabled.
See the `company-quickhelp-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-quickhelp-mode'.")

(custom-autoload 'company-quickhelp-mode "company-quickhelp" nil)

(autoload 'company-quickhelp-mode "company-quickhelp" "\
Toggle Company-Quickhelp-Local mode in all buffers.
With prefix ARG, enable Company-Quickhelp mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG is
omitted or nil.

Company-Quickhelp-Local mode is enabled in all buffers where
`company-quickhelp-local-mode' would do it.

See `company-quickhelp-local-mode' for more information on
Company-Quickhelp-Local mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "company-quickhelp" '("company-quickhelp-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-quickhelp-autoloads.el ends here
;;; company-quickhelp-terminal-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-quickhelp-terminal" "company-quickhelp-terminal.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-quickhelp-terminal.el

(defvar company-quickhelp-terminal-mode nil "\
Non-nil if Company-Quickhelp-Terminal mode is enabled.
See the `company-quickhelp-terminal-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-quickhelp-terminal-mode'.")

(custom-autoload 'company-quickhelp-terminal-mode "company-quickhelp-terminal" nil)

(autoload 'company-quickhelp-terminal-mode "company-quickhelp-terminal" "\
Minor mode 'company-quickhelp-terminal-mode'.

If called interactively, toggle `Company-Quickhelp-Terminal
mode'.  If the prefix argument is positive, enable the mode, and
if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "company-quickhelp-terminal" '("company-quickhelp-terminal--"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-quickhelp-terminal-autoloads.el ends here
;;; consult-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "consult" "consult.el" (0 0 0 0))
;;; Generated autoloads from consult.el

(autoload 'consult-multi-occur "consult" "\
Improved version of `multi-occur' based on `completing-read-multiple'.

See `multi-occur' for the meaning of the arguments BUFS, REGEXP and NLINES.

\(fn BUFS REGEXP &optional NLINES)" t nil)

(autoload 'consult-outline "consult" "\
Jump to an outline heading, obtained by matching against `outline-regexp'.

This command supports candidate preview.
The symbol at point is added to the future history." t nil)

(autoload 'consult-mark "consult" "\
Jump to a marker in the buffer-local `mark-ring'.

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history." t nil)

(autoload 'consult-global-mark "consult" "\
Jump to a marker in `global-mark-ring'.

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history." t nil)

(autoload 'consult-line "consult" "\
Search for a matching line and jump to the line beginning.

The default candidate is a non-empty line closest to point.
This command obeys narrowing. Optionally INITIAL input can be provided.
The symbol at point and the last `isearch-string' is added to the future history.

\(fn &optional INITIAL)" t nil)

(autoload 'consult-keep-lines "consult" "\
Select a subset of the lines in the current buffer with live preview.

The selected lines are kept and the other lines are deleted. When called
interactively, the lines selected are those that match the minibuffer input. In
order to match the inverse of the input, prefix the input with `! '. When
called from elisp, the filtering is performed by a FILTER function. This
command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input.

\(fn &optional FILTER INITIAL)" t nil)

(autoload 'consult-focus-lines "consult" "\
Hide or show lines using overlays.

The selected lines are shown and the other lines hidden. When called
interactively, the lines selected are those that match the minibuffer input. In
order to match the inverse of the input, prefix the input with `! '. With
optional prefix argument SHOW reveal the hidden lines. When called from elisp,
the filtering is performed by a FILTER function. This command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input.

\(fn &optional SHOW FILTER INITIAL)" t nil)

(autoload 'consult-goto-line "consult" "\
Read line number and jump to the line with preview.

The command respects narrowing and the settings
`consult-goto-line-numbers' and `consult-line-numbers-widen'." t nil)

(autoload 'consult-recent-file "consult" "\
Find recent using `completing-read'." t nil)

(autoload 'consult-file-externally "consult" "\
Open FILE externally using the default application of the system.

\(fn FILE)" t nil)

(autoload 'consult-completion-in-region "consult" "\
Prompt for completion of region in the minibuffer if non-unique.

The function is called with 4 arguments: START END COLLECTION PREDICATE.
The arguments and expected return value are as specified for
`completion-in-region'. Use as a value for `completion-in-region-function'.

\(fn START END COLLECTION &optional PREDICATE)" nil nil)

(autoload 'consult-mode-command "consult" "\
Run a command from any of the given MODES.

If no MODES are specified, use currently active major and minor modes.

\(fn &rest MODES)" t nil)

(autoload 'consult-yank "consult" "\
Select text from the kill ring and insert it." t nil)

(autoload 'consult-yank-pop "consult" "\
If there is a recent yank act like `yank-pop'.

Otherwise select text from the kill ring and insert it.
See `yank-pop' for the meaning of ARG.

\(fn &optional ARG)" t nil)

(autoload 'consult-yank-replace "consult" "\
Select text from the kill ring.

If there was no recent yank, insert the text.
Otherwise replace the just-yanked text with the selected text." t nil)

(autoload 'consult-register-window "consult" "\
Enhanced drop-in replacement for `register-preview'.

BUFFER is the window buffer.
SHOW-EMPTY must be t if the window should be shown for an empty register list.

\(fn BUFFER &optional SHOW-EMPTY)" nil nil)

(autoload 'consult-register-format "consult" "\
Enhanced preview of register REG.

This function can be used as `register-preview-function'.

\(fn REG)" nil nil)

(autoload 'consult-register "consult" "\
Load register and either jump to location or insert the stored text.

This command is useful to search the register contents. For quick access to
registers it is still recommended to use the register functions
`consult-register-load' and `consult-register-store' or the built-in built-in
register access functions. The command supports narrowing, see
`consult-register-narrow'. Marker positions are previewed. See
`jump-to-register' and `insert-register' for the meaning of prefix ARG.

\(fn &optional ARG)" t nil)

(autoload 'consult-register-load "consult" "\
Do what I mean with a REG.

For a window configuration, restore it. For a number or text, insert it. For a
location, jump to it. See `jump-to-register' and `insert-register' for the
meaning of prefix ARG.

\(fn REG &optional ARG)" t nil)

(autoload 'consult-register-store "consult" "\
Store register dependent on current context, showing an action menu.

With an active region, store/append/prepend the contents, optionally deleting
the region when a prefix ARG is given. With a numeric prefix ARG, store/add the
number. Otherwise store point, frameset, window or kmacro.

\(fn ARG)" t nil)

(autoload 'consult-bookmark "consult" "\
If bookmark NAME exists, open it, otherwise create a new bookmark with NAME.

The command supports preview of file bookmarks and narrowing. See the
variable `consult-bookmark-narrow' for the narrowing configuration.

\(fn NAME)" t nil)

(autoload 'consult-apropos "consult" "\
Select pattern and call `apropos'.

The default value of the completion is the symbol at point." t nil)

(autoload 'consult-complex-command "consult" "\
Select and evaluate command from the command history.

This command can act as a drop-in replacement for `repeat-complex-command'." t nil)

(autoload 'consult-history "consult" "\
Insert string from HISTORY of current buffer.

In order to select from a specific HISTORY, pass the history variable as argument.

\(fn &optional HISTORY)" t nil)

(autoload 'consult-isearch "consult" "\
Read a search string with completion from history.

This replaces the current search string if Isearch is active, and
starts a new Isearch session otherwise." t nil)

(autoload 'consult-minor-mode-menu "consult" "\
Enable or disable minor mode.

This is an alternative to `minor-mode-menu-from-indicator'." t nil)

(autoload 'consult-theme "consult" "\
Disable current themes and enable THEME from `consult-themes'.

The command supports previewing the currently selected theme.

\(fn THEME)" t nil)

(autoload 'consult-buffer "consult" "\
Enhanced `switch-to-buffer' command with support for virtual buffers.

The command supports recent files, bookmarks, views and project files as virtual
buffers. Buffers are previewed. Furthermore narrowing to buffers (b), files (f),
bookmarks (m) and project files (p) is supported via the corresponding keys. In
order to determine the project-specific files and buffers, the
`consult-project-root-function' is used. See `consult-buffer-sources' and
`consult--multi' for the configuration of the virtual buffer sources." t nil)

(autoload 'consult-buffer-other-window "consult" "\
Variant of `consult-buffer' which opens in other window." t nil)

(autoload 'consult-buffer-other-frame "consult" "\
Variant of `consult-buffer' which opens in other frame." t nil)

(autoload 'consult-kmacro "consult" "\
Run a chosen keyboard macro.

With prefix ARG, run the macro that many times.
Macros containing mouse clicks are omitted.

\(fn ARG)" t nil)

(autoload 'consult-imenu "consult" "\
Choose item from flattened `imenu' using `completing-read' with preview.

The command supports preview and narrowing. See the variable
`consult-imenu-config', which configures the narrowing.

See also `consult-project-imenu'." t nil)

(autoload 'consult-project-imenu "consult" "\
Choose item from the imenus of all buffers from the same project.

In order to determine the buffers belonging to the same project, the
`consult-project-root-function' is used. Only the buffers with the
same major mode as the current buffer are used. See also
`consult-imenu' for more details." t nil)

(autoload 'consult-grep "consult" "\
Search for regexp with grep in DIR with INITIAL input.

The input string is split, the first part of the string is passed to
the asynchronous grep process and the second part of the string is
passed to the completion-style filtering. The input string is split at
a punctuation character, which is given as the first character of the
input string. The format is similar to Perl-style regular expressions,
e.g., /regexp/. Furthermore command line options can be passed to
grep, specified behind --.

Example: #async-regexp -- grep-opts#filter-string

The symbol at point is added to the future history. If `consult-grep'
is called interactively with a prefix argument, the user can specify
the directory to search in. By default the project directory is used
if `consult-project-root-function' is defined and returns non-nil.
Otherwise the `default-directory' is searched.

\(fn &optional DIR INITIAL)" t nil)

(autoload 'consult-git-grep "consult" "\
Search for regexp with grep in DIR with INITIAL input.

See `consult-grep' for more details.

\(fn &optional DIR INITIAL)" t nil)

(autoload 'consult-ripgrep "consult" "\
Search for regexp with rg in DIR with INITIAL input.

See `consult-grep' for more details.

\(fn &optional DIR INITIAL)" t nil)

(autoload 'consult-find "consult" "\
Search for regexp with find in DIR with INITIAL input.

The find process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search.

\(fn &optional DIR INITIAL)" t nil)

(autoload 'consult-locate "consult" "\
Search for regexp with locate with INITIAL input.

The locate process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search.

\(fn &optional INITIAL)" t nil)

(autoload 'consult-man "consult" "\
Search for regexp with man with INITIAL input.

The man process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search.

\(fn &optional INITIAL)" t nil)

(register-definition-prefixes "consult" '("consult-"))

;;;***

;;;### (autoloads nil "consult-compile" "consult-compile.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from consult-compile.el

(autoload 'consult-compile-error "consult-compile" "\
Jump to a compilation error in the current buffer.

This command collects entries from compilation buffers and grep
buffers related to the current buffer.  The command supports
preview of the currently selected error." t nil)

(register-definition-prefixes "consult-compile" '("consult-compile--"))

;;;***

;;;### (autoloads nil "consult-flymake" "consult-flymake.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from consult-flymake.el

(autoload 'consult-flymake "consult-flymake" "\
Jump to Flymake diagnostic." t nil)

(register-definition-prefixes "consult-flymake" '("consult-flymake--"))

;;;***

;;;### (autoloads nil "consult-icomplete" "consult-icomplete.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from consult-icomplete.el

(register-definition-prefixes "consult-icomplete" '("consult-icomplete--refresh"))

;;;***

;;;### (autoloads nil "consult-selectrum" "consult-selectrum.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from consult-selectrum.el

(register-definition-prefixes "consult-selectrum" '("consult-selectrum--"))

;;;***

;;;### (autoloads nil "consult-xref" "consult-xref.el" (0 0 0 0))
;;; Generated autoloads from consult-xref.el

(autoload 'consult-xref "consult-xref" "\
Show xrefs with preview in the minibuffer.

This function can be used for `xref-show-xrefs-function'.
See `xref-show-xrefs-function' for the description of the
FETCHER and ALIST arguments.

\(fn FETCHER &optional ALIST)" nil nil)

(register-definition-prefixes "consult-xref" '("consult-xref--"))

;;;***

;;;### (autoloads nil nil ("consult-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; consult-autoloads.el ends here
;;; counsel-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "counsel" "counsel.el" (0 0 0 0))
;;; Generated autoloads from counsel.el

(autoload 'counsel-company "counsel" "\
Complete using `company-candidates'." t nil)

(autoload 'counsel-irony "counsel" "\
Inline C/C++ completion using Irony." t nil)

(autoload 'counsel-describe-variable "counsel" "\
Forward to `describe-variable'.

Variables declared using `defcustom' are highlighted according to
`ivy-highlight-face'." t nil)

(autoload 'counsel-describe-function "counsel" "\
Forward to `describe-function'.

Interactive functions (i.e., commands) are highlighted according
to `ivy-highlight-face'." t nil)

(autoload 'counsel-describe-symbol "counsel" "\
Forward to `describe-symbol'." t nil)

(autoload 'counsel-set-variable "counsel" "\
Set a variable SYM, with completion.

When the selected variable is a `defcustom' with the type boolean
or radio, offer completion of all possible values.

Otherwise, offer a variant of `eval-expression', with the initial
input corresponding to the chosen variable.

With a prefix arg, restrict list to variables defined using
`defcustom'.

\(fn SYM)" t nil)

(autoload 'counsel-apropos "counsel" "\
Show all matching symbols.
See `apropos' for further information on what is considered
a symbol and how to search for them." t nil)

(autoload 'counsel-info-lookup-symbol "counsel" "\
Forward SYMBOL to `info-lookup-symbol' with ivy completion.
With prefix arg MODE a query for the symbol help mode is offered.

\(fn SYMBOL &optional MODE)" t nil)

(autoload 'counsel-M-x "counsel" "\
Ivy version of `execute-extended-command'.
Optional INITIAL-INPUT is the initial input in the minibuffer.
This function integrates with either the `amx' or `smex' package
when available, in that order of precedence.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-command-history "counsel" "\
Show the history of commands." t nil)

(autoload 'counsel-load-library "counsel" "\
Load a selected the Emacs Lisp library.
The libraries are offered from `load-path'." t nil)

(autoload 'counsel-find-library "counsel" "\
Visit a selected the Emacs Lisp library.
The libraries are offered from `load-path'." t nil)

(autoload 'counsel-load-theme "counsel" "\
Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'." t nil)

(autoload 'counsel-descbinds "counsel" "\
Show a list of all defined keys and their definitions.
If non-nil, show only bindings that start with PREFIX.
BUFFER defaults to the current one.

\(fn &optional PREFIX BUFFER)" t nil)

(autoload 'counsel-describe-face "counsel" "\
Completion for `describe-face'." t nil)

(autoload 'counsel-faces "counsel" "\
Complete faces with preview.
Actions are provided by default for describing or customizing the
selected face." t nil)

(autoload 'counsel-git "counsel" "\
Find file in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-git-grep "counsel" "\
Grep for a string in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
When CMD is a string, use it as a \"git grep\" command.
When CMD is non-nil, prompt for a specific \"git grep\" command.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY CMD)" t nil)

(autoload 'counsel-git-stash "counsel" "\
Search through all available git stashes." t nil)

(autoload 'counsel-git-change-worktree "counsel" "\
Find the file corresponding to the current buffer on a different worktree." t nil)

(autoload 'counsel-git-checkout "counsel" "\
Call the \"git checkout\" command." t nil)

(autoload 'counsel-git-log "counsel" "\
Call the \"git log --grep\" shell command." t nil)

(autoload 'counsel-find-file "counsel" "\
Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil)

(autoload 'counsel-dired "counsel" "\
Forward to `dired'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-recentf "counsel" "\
Find a file on `recentf-list'." t nil)

(autoload 'counsel-buffer-or-recentf "counsel" "\
Find a buffer visiting a file or file on `recentf-list'." t nil)

(autoload 'counsel-bookmark "counsel" "\
Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist." t nil)

(autoload 'counsel-bookmarked-directory "counsel" "\
Ivy interface for bookmarked directories.

With a prefix argument, this command creates a new bookmark which points to the
current value of `default-directory'." t nil)

(autoload 'counsel-file-register "counsel" "\
Search file in register.

You cannot use Emacs' normal register commands to create file
registers.  Instead you must use the `set-register' function like
so: `(set-register ?i \"/home/eric/.emacs.d/init.el\")'.  Now you
can use `C-x r j i' to open that file." t nil)

(autoload 'counsel-locate-action-extern "counsel" "\
Pass X to `xdg-open' or equivalent command via the shell.

\(fn X)" t nil)

(autoload 'counsel-locate "counsel" "\
Call a \"locate\" style shell command.
INITIAL-INPUT can be given as the initial minibuffer input.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-tracker "counsel" nil t nil)

(autoload 'counsel-fzf "counsel" "\
Open a file using the fzf shell command.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
FZF-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY FZF-PROMPT)" t nil)

(autoload 'counsel-dpkg "counsel" "\
Call the \"dpkg\" shell command." t nil)

(autoload 'counsel-rpm "counsel" "\
Call the \"rpm\" shell command." t nil)

(autoload 'counsel-file-jump "counsel" "\
Jump to a file below the current directory.
List all files within the current directory or any of its sub-directories.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil)

(autoload 'counsel-dired-jump "counsel" "\
Jump to a directory (see `dired-jump') below the current directory.
List all sub-directories within the current directory.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil)

(autoload 'counsel-ag "counsel" "\
Grep for a string in a root directory using `ag'.

By default, the root directory is the first directory containing
a .git subdirectory.

INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.
CALLER is passed to `ivy-read'.

With a `\\[universal-argument]' prefix argument, prompt for INITIAL-DIRECTORY.
With a `\\[universal-argument] \\[universal-argument]' prefix argument, prompt additionally for EXTRA-AG-ARGS.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-AG-ARGS AG-PROMPT &key CALLER)" t nil)

(autoload 'counsel-pt "counsel" "\
Grep for a string in the current directory using pt.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-pt-base-command' instead of
`counsel-ag-base-command'.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-ack "counsel" "\
Grep for a string in the current directory using ack.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-ack-base-command' replacing
`counsel-ag-base-command'.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-rg "counsel" "\
Grep for a string in the current directory using `rg'.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-RG-ARGS string, if non-nil, is appended to `counsel-rg-base-command'.
RG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

Example input with inclusion and exclusion file patterns:
    require i -- -g*.el

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-RG-ARGS RG-PROMPT)" t nil)

(autoload 'counsel-grep "counsel" "\
Grep for a string in the file visited by the current buffer.
When non-nil, INITIAL-INPUT is the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-grep-backward "counsel" "\
Grep for a string in the file visited by the current buffer going
backward similar to `swiper-backward'. When non-nil, INITIAL-INPUT is
the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-grep-or-swiper "counsel" "\
Call `swiper' for small buffers and `counsel-grep' for large ones.
When non-nil, INITIAL-INPUT is the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-grep-or-swiper-backward "counsel" "\
Call `swiper-backward' for small buffers and `counsel-grep-backward' for
large ones.  When non-nil, INITIAL-INPUT is the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-recoll "counsel" "\
Search for a string in the recoll database.
You'll be given a list of files that match.
Selecting a file will launch `swiper' for that file.
INITIAL-INPUT can be given as the initial minibuffer input.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel--org-get-tags "counsel" nil nil nil)

(autoload 'counsel-org-tag "counsel" "\
Add or remove tags in `org-mode'." t nil)

(autoload 'counsel-org-tag-agenda "counsel" "\
Set tags for the current agenda item." t nil)

(defalias 'counsel-org-goto #'counsel-outline)

(autoload 'counsel-org-goto-all "counsel" "\
Go to a different location in any org file." t nil)

(autoload 'counsel-org-file "counsel" "\
Browse all attachments for current Org file." t nil)

(autoload 'counsel-org-entity "counsel" "\
Complete Org entities using Ivy." t nil)

(autoload 'counsel-org-capture "counsel" "\
Capture something." t nil)

(autoload 'counsel-org-agenda-headlines "counsel" "\
Choose from headers of `org-mode' files in the agenda." t nil)

(autoload 'counsel-org-link "counsel" "\
Insert a link to an headline with completion." t nil)

(autoload 'counsel-mark-ring "counsel" "\
Browse `mark-ring' interactively.
Obeys `widen-automatically', which see." t nil)

(autoload 'counsel-evil-marks "counsel" "\
Ivy replacement for `evil-show-marks'.
By default, this function respects `counsel-evil-marks-exclude-registers'.
When ARG is non-nil, display all active evil registers.

\(fn &optional ARG)" t nil)

(autoload 'counsel-package "counsel" "\
Install or delete packages.

Packages not currently installed are prefixed with \"+\", and
selecting one of these will try to install it.
Packages currently installed are prefixed with \"-\", and
selecting one of these will try to delete it.

Additional actions:\\<ivy-minibuffer-map>

  \\[ivy-dispatching-done] d: Describe package
  \\[ivy-dispatching-done] h: Visit package's homepage" t nil)

(autoload 'counsel-tmm "counsel" "\
Text-mode emulation of looking and choosing from a menu bar." t nil)

(autoload 'counsel-yank-pop "counsel" "\
Ivy replacement for `yank-pop'.
With a plain prefix argument (\\[universal-argument]),
temporarily toggle the value of `counsel-yank-pop-after-point'.
Any other value of ARG has the same meaning as in `yank-pop', but
`counsel-yank-pop-preselect-last' determines its default value.
See also `counsel-yank-pop-filter' for how to filter candidates.

Note: Duplicate elements of `kill-ring' are always deleted.

\(fn &optional ARG)" t nil)

(autoload 'counsel-register "counsel" "\
Interactively choose a register." t nil)

(autoload 'counsel-evil-registers "counsel" "\
Ivy replacement for `evil-show-registers'." t nil)

(autoload 'counsel-imenu "counsel" "\
Jump to a buffer position indexed by imenu." t nil)

(autoload 'counsel-list-processes "counsel" "\
Offer completion for `process-list'.
The default action deletes the selected process.
An extra action allows to switch to the process buffer." t nil)

(autoload 'counsel-minibuffer-history "counsel" "\
Browse minibuffer history." t nil)

(autoload 'counsel-esh-history "counsel" "\
Browse Eshell history." t nil)

(autoload 'counsel-shell-history "counsel" "\
Browse shell history." t nil)

(autoload 'counsel-slime-repl-history "counsel" "\
Browse Slime REPL history." t nil)

(autoload 'counsel-hydra-heads "counsel" "\
Call a head of the current/last hydra." t nil)

(autoload 'counsel-semantic "counsel" "\
Jump to a semantic tag in the current buffer." t nil)

(autoload 'counsel-semantic-or-imenu "counsel" nil t nil)

(autoload 'counsel-outline "counsel" "\
Jump to an outline heading with completion." t nil)

(autoload 'counsel-ibuffer "counsel" "\
Use ibuffer to switch to another buffer.
NAME specifies the name of the buffer (defaults to \"*Ibuffer*\").

\(fn &optional NAME)" t nil)

(autoload 'counsel-switch-to-shell-buffer "counsel" "\
Switch to a shell buffer, or create one." t nil)

(autoload 'counsel-unicode-char "counsel" "\
Insert COUNT copies of a Unicode character at point.
COUNT defaults to 1.

\(fn &optional COUNT)" t nil)

(autoload 'counsel-colors-emacs "counsel" "\
Show a list of all supported colors for a particular frame.

You can insert or kill the name or hexadecimal RGB value of the
selected color." t nil)

(autoload 'counsel-colors-web "counsel" "\
Show a list of all W3C web colors for use in CSS.

You can insert or kill the name or hexadecimal RGB value of the
selected color." t nil)

(autoload 'counsel-fonts "counsel" "\
Show a list of all supported font families for a particular frame.

You can insert or kill the name of the selected font." t nil)

(autoload 'counsel-kmacro "counsel" "\
Interactively choose and run a keyboard macro.

With prefix argument, run macro that many times.

Macros are run using the current value of `kmacro-counter-value'
and their respective counter format. Displayed next to each macro is
the counter's format and initial value.

One can use actions to copy the counter format or initial counter
value of a macro, using them for a new macro." t nil)

(autoload 'counsel-geiser-doc-look-up-manual "counsel" "\
Search Scheme documentation." t nil)

(autoload 'counsel-rhythmbox "counsel" "\
Choose a song from the Rhythmbox library to play or enqueue.

\(fn &optional ARG)" t nil)

(autoload 'counsel-linux-app "counsel" "\
Launch a Linux desktop application, similar to Alt-<F2>.
When ARG is non-nil, ignore NoDisplay property in *.desktop files.

\(fn &optional ARG)" t nil)

(autoload 'counsel-wmctrl "counsel" "\
Select a desktop window using wmctrl." t nil)

(autoload 'counsel-switch-buffer "counsel" "\
Switch to another buffer.
Display a preview of the selected ivy completion candidate buffer
in the current window." t nil)

(autoload 'counsel-switch-buffer-other-window "counsel" "\
Switch to another buffer in another window.
Display a preview of the selected ivy completion candidate buffer
in the current window." t nil)

(autoload 'counsel-compile "counsel" "\
Call `compile' completing with smart suggestions, optionally for DIR.

Additional actions:

\\{counsel-compile-map}

\(fn &optional DIR)" t nil)

(autoload 'counsel-compile-env "counsel" "\
Update `counsel-compile-env' interactively." t nil)

(autoload 'counsel-minor "counsel" "\
Enable or disable minor mode.

Disabled minor modes are prefixed with \"+\", and
selecting one of these will enable it.
Enabled minor modes are prefixed with \"-\", and
selecting one of these will enable it.

Additional actions:\\<ivy-minibuffer-map>

  \\[ivy-dispatching-done] d: Go to minor mode definition
  \\[ivy-dispatching-done] h: Describe minor mode" t nil)

(autoload 'counsel-major "counsel" nil t nil)

(autoload 'counsel-compilation-errors "counsel" "\
Compilation errors." t nil)

(autoload 'counsel-flycheck "counsel" "\
Flycheck errors." t nil)

(defvar counsel-mode nil "\
Non-nil if Counsel mode is enabled.
See the `counsel-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `counsel-mode'.")

(custom-autoload 'counsel-mode "counsel" nil)

(autoload 'counsel-mode "counsel" "\
Toggle Counsel mode on or off.
Turn Counsel mode on if ARG is positive, off otherwise. Counsel
mode remaps built-in emacs functions that have counsel
replacements.

Local bindings (`counsel-mode-map'):
\\{counsel-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "counsel" '("counsel-" "ivy-function-called-at-point" "tmm-km-list"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; counsel-autoloads.el ends here
;;; counsel-etags-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "counsel-etags" "counsel-etags.el" (0 0 0 0))
;;; Generated autoloads from counsel-etags.el

(autoload 'counsel-etags-guess-program "counsel-etags" "\
Guess path from its EXECUTABLE-NAME on Windows.
Return nil if it's not found.

\(fn EXECUTABLE-NAME)" nil nil)

(autoload 'counsel-etags-version "counsel-etags" "\
Return version." nil nil)

(autoload 'counsel-etags-get-hostname "counsel-etags" "\
Reliable way to get current hostname.
`(getenv \"HOSTNAME\")' won't work because $HOSTNAME is NOT an
 environment variable.
`system-name' won't work because /etc/hosts could be modified" nil nil)

(autoload 'counsel-etags-async-shell-command "counsel-etags" "\
Execute string COMMAND and create TAGS-FILE asynchronously.

\(fn COMMAND TAGS-FILE)" nil nil)

(autoload 'counsel-etags-exuberant-ctags-p "counsel-etags" "\
If CTAGS-PROGRAM is Exuberant Ctags.

\(fn CTAGS-PROGRAM)" nil nil)

(autoload 'counsel-etags-universal-ctags-p "counsel-etags" "\
If CTAGS-PROGRAM is Universal Ctags.

\(fn CTAGS-PROGRAM)" nil nil)

(autoload 'counsel-etags-scan-dir-internal "counsel-etags" "\
Create tags file from SRC-DIR.

\(fn SRC-DIR)" nil nil)

(autoload 'counsel-etags-directory-p "counsel-etags" "\
Does directory of current file match REGEX?

\(fn REGEX)" nil nil)

(autoload 'counsel-etags-filename-p "counsel-etags" "\
Does current file match REGEX?

\(fn REGEX)" nil nil)

(autoload 'counsel-etags-push-marker-stack "counsel-etags" "\
Save current position." nil nil)

(autoload 'counsel-etags-find-tag-name-default "counsel-etags" "\
Find tag at point." nil nil)

(autoload 'counsel-etags-word-at-point "counsel-etags" "\
Get word at point.  PREDICATE should return t on testing word character.

For example, get a word when dot character is part of word,

   (counsel-etags-word-at-point (lambda (c)
                                  (or (= c ?.)
                                      (and (>= c ?0) (<= c ?9))
                                      (and (>= c ?A) (<= c ?Z))
                                      (and (>= c ?a) (<= c ?z)))))

\(fn PREDICATE)" nil nil)

(autoload 'counsel-etags-scan-code "counsel-etags" "\
Use Ctags to scan code at DIR.

\(fn &optional DIR)" t nil)

(autoload 'counsel-etags-list-tag "counsel-etags" "\
List all tags.  Tag is fuzzy and case insensitively matched." t nil)

(autoload 'counsel-etags-imenu-default-create-index-function "counsel-etags" "\
Create an index alist for the definitions in the current buffer." nil nil)

(autoload 'counsel-etags-list-tag-in-current-file "counsel-etags" "\
List tags in current file." t nil)

(autoload 'counsel-etags-find-tag "counsel-etags" "\
Find tag in two step.
Step 1, user need input regex to fuzzy and case insensitively match tag.
Any tag whose sub-string matches regex will be listed.

Step 2, user keeps filtering tags." t nil)

(autoload 'counsel-etags-find-tag-at-point "counsel-etags" "\
Find tag using tagname at point.
Please note parsing tags file containing line with 2K characters could be slow.
That's the known issue of Emacs Lisp.  The program itself is perfectly fine." t nil)

(autoload 'counsel-etags-recent-tag "counsel-etags" "\
Find tag using tagname from `counsel-etags-tag-history'." t nil)

(autoload 'counsel-etags-virtual-update-tags "counsel-etags" "\
Scan code and create tags file again.
It's the interface used by other hooks or commands.
The tags updating might not happen." t nil)

(autoload 'counsel-etags-grep "counsel-etags" "\
Grep at project root directory or current directory.
Try to find best grep program (ripgrep, grep...) automatically.
Extended regex like (pattern1|pattern2) is used.
If DEFAULT-KEYWORD is not nil, it's used as grep keyword.
If HINT is not nil, it's used as grep hint.
ROOT is root directory to grep.
If SHOW-KEYWORD-P is t, show the keyword in the minibuffer.

\(fn &optional DEFAULT-KEYWORD HINT ROOT SHOW-KEYWORD-P)" t nil)

(autoload 'counsel-etags-grep-current-directory "counsel-etags" "\
Grep current directory or LEVEL up parent directory.

\(fn &optional LEVEL)" t nil)

(autoload 'counsel-etags-update-tags-force "counsel-etags" "\
Update current tags file using default implementation.
If FORCED-TAGS-FILE is nil, the updating process might now happen.

\(fn &optional FORCED-TAGS-FILE)" t nil)

(autoload 'counsel-etags-tag-line "counsel-etags" "\
One line in tag file using CODE-SNIPPET, TAG-NAME, LINE-NUMBER, and BYTE-OFFSET.

\(fn CODE-SNIPPET TAG-NAME LINE-NUMBER &optional BYTE-OFFSET)" nil nil)

(autoload 'counsel-etags-append-to-tags-file "counsel-etags" "\
Append SECTIONS into TAGS-FILE.
Each section is a pair of file and tags content in that file.
File can be url template like \"https://developer.mozilla.org/en-US/docs/Web/API/%s\".
The `counsel-etags-browse-url-function' is used to open the url.

\(fn SECTIONS TAGS-FILE)" nil nil)

(register-definition-prefixes "counsel-etags" '("counsel-etags-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; counsel-etags-autoloads.el ends here
;;; cpputils-cmake-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cpputils-cmake" "cpputils-cmake.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from cpputils-cmake.el

(autoload 'cppcm-get-exe-path-current-buffer "cpputils-cmake" nil t nil)

(autoload 'cppcm-version "cpputils-cmake" nil t nil)

(autoload 'cppcm-compile "cpputils-cmake" "\
Compile the executable/library in current directory,
default compile command or compile in the build directory.
You can specify the sequence which compile is default
by customize `cppcm-compile-list'.

\(fn &optional PREFIX)" t nil)

(autoload 'cppcm-recompile "cpputils-cmake" "\
Run 'make clean && compile'." t nil)

(autoload 'cppcm-reload-all "cpputils-cmake" "\
Reload and reproduce everything." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cpputils-cmake" '("cppcm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cpputils-cmake-autoloads.el ends here
;;; csharp-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "csharp-compilation" "csharp-compilation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from csharp-compilation.el

(register-definition-prefixes "csharp-compilation" '("csharp-"))

;;;***

;;;### (autoloads nil "csharp-mode" "csharp-mode.el" (0 0 0 0))
;;; Generated autoloads from csharp-mode.el

(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

(defvar csharp-mode-hook nil "\
*Hook called by `csharp-mode'.")

(custom-autoload 'csharp-mode-hook "csharp-mode" t)

(autoload 'csharp-mode "csharp-mode" "\
Major mode for editing Csharp code.

Key bindings:
\\{csharp-mode-map}

\(fn)" t nil)

(register-definition-prefixes "csharp-mode" '("codedoc-font-lock-" "csharp-"))

;;;***

;;;### (autoloads nil "csharp-tree-sitter" "csharp-tree-sitter.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from csharp-tree-sitter.el

(autoload 'csharp-tree-sitter-mode "csharp-tree-sitter" "\
Major mode for editing Csharp code.

Key bindings:
\\{csharp-tree-sitter-mode-map}

\(fn)" t nil)

(register-definition-prefixes "csharp-tree-sitter" '("csharp-" "tree-sitter-indent-csharp-tree-sitter-scopes"))

;;;***

;;;### (autoloads nil nil ("csharp-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; csharp-mode-autoloads.el ends here
;;; csv-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "csv-mode" "csv-mode.el" (0 0 0 0))
;;; Generated autoloads from csv-mode.el

(autoload 'csv-mode "csv-mode" "\
Major mode for editing files of comma-separated value type.

CSV mode is derived from `text-mode', and runs `text-mode-hook' before
running `csv-mode-hook'.  It turns `auto-fill-mode' off by default.
CSV mode can be customized by user options in the CSV customization
group.  The separators are specified by the value of `csv-separators'.

CSV mode commands ignore blank lines and comment lines beginning with
the value of `csv-comment-start', which delimit \"paragraphs\".
\"Sexp\" is re-interpreted to mean \"field\", so that `forward-sexp'
\(\\[forward-sexp]), `kill-sexp' (\\[kill-sexp]), etc. all apply to fields.
Standard comment commands apply, such as `comment-dwim' (\\[comment-dwim]).

If `font-lock-mode' is enabled then separators, quoted values and
comment lines are highlighted using respectively `csv-separator-face',
`font-lock-string-face' and `font-lock-comment-face'.

The user interface (UI) for CSV mode commands is similar to that of
the standard commands `sort-fields' and `sort-numeric-fields', except
that if there is no prefix argument then the UI prompts for the field
index or indices.  In `transient-mark-mode' only: if the region is not
set then the UI attempts to set it to include all consecutive CSV
records around point, and prompts for confirmation; if there is no
prefix argument then the UI prompts for it, offering as a default the
index of the field containing point if the region was not set
explicitly.  The region set automatically is delimited by blank lines
and comment lines, and the number of header lines at the beginning of
the region given by the value of `csv-header-lines' are skipped.

Sort order is controlled by `csv-descending'.

CSV mode provides the following specific keyboard key bindings:

\\{csv-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(add-to-list 'auto-mode-alist '("\\.tsv\\'" . tsv-mode))

(autoload 'tsv-mode "csv-mode" "\
Major mode for editing files of tab-separated value type.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "csv-mode" '("csv-" "tsv-")))

;;;***

;;;### (autoloads nil "csv-mode-tests" "csv-mode-tests.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from csv-mode-tests.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "csv-mode-tests" '("csv-mode-tests--align-fields")))

;;;***

;;;### (autoloads nil nil ("csv-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; csv-mode-autoloads.el ends here
;;; dante-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dante" "dante.el" (0 0 0 0))
;;; Generated autoloads from dante.el

(autoload 'dante-mode "dante" "\
Minor mode for Dante.

If called interactively, enable Dante mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

`dante-mode' takes one optional (prefix) argument.
Interactively with no prefix argument, it toggles dante.
A prefix argument enables dante if the argument is positive,
and disables it otherwise.

When called from Lisp, the `dante-mode' toggles dante if the
argument is `toggle', disables dante if the argument is a
non-positive integer, and enables dante otherwise (including
if the argument is omitted or nil or a positive integer).

\\{dante-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dante" '("check-balanced-parens" "dante-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dante-autoloads.el ends here
;;; dap-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dap-chrome" "dap-chrome.el" (0 0 0 0))
;;; Generated autoloads from dap-chrome.el

(register-definition-prefixes "dap-chrome" '("dap-chrome-"))

;;;***

;;;### (autoloads nil "dap-codelldb" "dap-codelldb.el" (0 0 0 0))
;;; Generated autoloads from dap-codelldb.el

(register-definition-prefixes "dap-codelldb" '("dap-codelldb-"))

;;;***

;;;### (autoloads nil "dap-cpptools" "dap-cpptools.el" (0 0 0 0))
;;; Generated autoloads from dap-cpptools.el

(register-definition-prefixes "dap-cpptools" '("dap-cpptools-"))

;;;***

;;;### (autoloads nil "dap-edge" "dap-edge.el" (0 0 0 0))
;;; Generated autoloads from dap-edge.el

(register-definition-prefixes "dap-edge" '("dap-edge-"))

;;;***

;;;### (autoloads nil "dap-elixir" "dap-elixir.el" (0 0 0 0))
;;; Generated autoloads from dap-elixir.el

(register-definition-prefixes "dap-elixir" '("dap-elixir--populate-start-file-args"))

;;;***

;;;### (autoloads nil "dap-firefox" "dap-firefox.el" (0 0 0 0))
;;; Generated autoloads from dap-firefox.el

(register-definition-prefixes "dap-firefox" '("dap-firefox-"))

;;;***

;;;### (autoloads nil "dap-gdb-lldb" "dap-gdb-lldb.el" (0 0 0 0))
;;; Generated autoloads from dap-gdb-lldb.el

(register-definition-prefixes "dap-gdb-lldb" '("dap-gdb-lldb-"))

;;;***

;;;### (autoloads nil "dap-go" "dap-go.el" (0 0 0 0))
;;; Generated autoloads from dap-go.el

(register-definition-prefixes "dap-go" '("dap-go-"))

;;;***

;;;### (autoloads nil "dap-hydra" "dap-hydra.el" (0 0 0 0))
;;; Generated autoloads from dap-hydra.el

(autoload 'dap-hydra "dap-hydra" "\
Run `dap-hydra/body'." t nil)

(register-definition-prefixes "dap-hydra" '("dap-hydra"))

;;;***

;;;### (autoloads nil "dap-launch" "dap-launch.el" (0 0 0 0))
;;; Generated autoloads from dap-launch.el

(register-definition-prefixes "dap-launch" '("dap-launch-"))

;;;***

;;;### (autoloads nil "dap-lldb" "dap-lldb.el" (0 0 0 0))
;;; Generated autoloads from dap-lldb.el

(register-definition-prefixes "dap-lldb" '("dap-lldb-"))

;;;***

;;;### (autoloads nil "dap-mode" "dap-mode.el" (0 0 0 0))
;;; Generated autoloads from dap-mode.el

(autoload 'dap-debug "dap-mode" "\
Run debug configuration DEBUG-ARGS.

If DEBUG-ARGS is not specified the configuration is generated
after selecting configuration template.

\(fn DEBUG-ARGS)" t nil)

(defvar dap-mode nil "\
Non-nil if Dap mode is enabled.
See the `dap-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-mode'.")

(custom-autoload 'dap-mode "dap-mode" nil)

(autoload 'dap-mode "dap-mode" "\
Global minor mode for DAP mode.

If called interactively, toggle `Dap mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(defvar dap-auto-configure-mode nil "\
Non-nil if Dap-Auto-Configure mode is enabled.
See the `dap-auto-configure-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-auto-configure-mode'.")

(custom-autoload 'dap-auto-configure-mode "dap-mode" nil)

(autoload 'dap-auto-configure-mode "dap-mode" "\
Auto configure dap minor mode.

If called interactively, toggle `Dap-Auto-Configure mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "dap-mode" '("dap-"))

;;;***

;;;### (autoloads nil "dap-mouse" "dap-mouse.el" (0 0 0 0))
;;; Generated autoloads from dap-mouse.el

(defvar dap-tooltip-mode nil "\
Non-nil if Dap-Tooltip mode is enabled.
See the `dap-tooltip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-tooltip-mode'.")

(custom-autoload 'dap-tooltip-mode "dap-mouse" nil)

(autoload 'dap-tooltip-mode "dap-mouse" "\
Toggle the display of GUD tooltips.

If called interactively, toggle `Dap-Tooltip mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "dap-mouse" '("dap-"))

;;;***

;;;### (autoloads nil "dap-netcore" "dap-netcore.el" (0 0 0 0))
;;; Generated autoloads from dap-netcore.el

(register-definition-prefixes "dap-netcore" '("dap-netcore-"))

;;;***

;;;### (autoloads nil "dap-node" "dap-node.el" (0 0 0 0))
;;; Generated autoloads from dap-node.el

(register-definition-prefixes "dap-node" '("dap-node-"))

;;;***

;;;### (autoloads nil "dap-overlays" "dap-overlays.el" (0 0 0 0))
;;; Generated autoloads from dap-overlays.el

(register-definition-prefixes "dap-overlays" '("dap-overlays-"))

;;;***

;;;### (autoloads nil "dap-php" "dap-php.el" (0 0 0 0))
;;; Generated autoloads from dap-php.el

(register-definition-prefixes "dap-php" '("dap-php-"))

;;;***

;;;### (autoloads nil "dap-pwsh" "dap-pwsh.el" (0 0 0 0))
;;; Generated autoloads from dap-pwsh.el

(register-definition-prefixes "dap-pwsh" '("dap-pwsh-"))

;;;***

;;;### (autoloads nil "dap-python" "dap-python.el" (0 0 0 0))
;;; Generated autoloads from dap-python.el

(register-definition-prefixes "dap-python" '("dap-python-"))

;;;***

;;;### (autoloads nil "dap-ruby" "dap-ruby.el" (0 0 0 0))
;;; Generated autoloads from dap-ruby.el

(register-definition-prefixes "dap-ruby" '("dap-ruby-"))

;;;***

;;;### (autoloads nil "dap-ui" "dap-ui.el" (0 0 0 0))
;;; Generated autoloads from dap-ui.el

(defvar dap-ui-mode nil "\
Non-nil if Dap-Ui mode is enabled.
See the `dap-ui-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-ui-mode'.")

(custom-autoload 'dap-ui-mode "dap-ui" nil)

(autoload 'dap-ui-mode "dap-ui" "\
Displaying DAP visuals.

If called interactively, toggle `Dap-Ui mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'dap-ui-breakpoints-list "dap-ui" "\
List breakpoints." t nil)

(defvar dap-ui-controls-mode nil "\
Non-nil if Dap-Ui-Controls mode is enabled.
See the `dap-ui-controls-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-ui-controls-mode'.")

(custom-autoload 'dap-ui-controls-mode "dap-ui" nil)

(autoload 'dap-ui-controls-mode "dap-ui" "\
Displaying DAP visuals.

If called interactively, toggle `Dap-Ui-Controls mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'dap-ui-sessions "dap-ui" "\
Show currently active sessions." t nil)

(autoload 'dap-ui-locals "dap-ui" nil t nil)

(autoload 'dap-ui-show-many-windows "dap-ui" "\
Show auto configured feature windows." t nil)

(autoload 'dap-ui-hide-many-windows "dap-ui" "\
Hide all debug windows when sessions are dead." t nil)

(autoload 'dap-ui-repl "dap-ui" "\
Start an adapter-specific REPL.
This could be used to evaluate JavaScript in a browser, to
evaluate python in the context of the debugee, ...." t nil)

(register-definition-prefixes "dap-ui" '("dap-"))

;;;***

;;;### (autoloads nil "dap-utils" "dap-utils.el" (0 0 0 0))
;;; Generated autoloads from dap-utils.el

(register-definition-prefixes "dap-utils" '("dap-utils-"))

;;;***

;;;### (autoloads nil "dap-variables" "dap-variables.el" (0 0 0 0))
;;; Generated autoloads from dap-variables.el

(register-definition-prefixes "dap-variables" '("dap-variables-"))

;;;***

;;;### (autoloads nil "dapui" "dapui.el" (0 0 0 0))
;;; Generated autoloads from dapui.el

(autoload 'dapui-loaded-sources "dapui" nil t nil)

(register-definition-prefixes "dapui" '("dapui-"))

;;;***

;;;### (autoloads nil nil ("dap-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dap-mode-autoloads.el ends here
;;; dash-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dash" "dash.el" (0 0 0 0))
;;; Generated autoloads from dash.el

(autoload 'dash-fontify-mode "dash" "\
Toggle fontification of Dash special variables.

If called interactively, toggle `Dash-Fontify mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Dash-Fontify mode is a buffer-local minor mode intended for Emacs
Lisp buffers.  Enabling it causes the special variables bound in
anaphoric Dash macros to be fontified.  These anaphoras include
`it', `it-index', `acc', and `other'.  In older Emacs versions
which do not dynamically detect macros, Dash-Fontify mode
additionally fontifies Dash macro calls.

See also `dash-fontify-mode-lighter' and
`global-dash-fontify-mode'.

\(fn &optional ARG)" t nil)

(put 'global-dash-fontify-mode 'globalized-minor-mode t)

(defvar global-dash-fontify-mode nil "\
Non-nil if Global Dash-Fontify mode is enabled.
See the `global-dash-fontify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-dash-fontify-mode'.")

(custom-autoload 'global-dash-fontify-mode "dash" nil)

(autoload 'global-dash-fontify-mode "dash" "\
Toggle Dash-Fontify mode in all buffers.
With prefix ARG, enable Global Dash-Fontify mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG
is omitted or nil.

Dash-Fontify mode is enabled in all buffers where
`dash--turn-on-fontify-mode' would do it.

See `dash-fontify-mode' for more information on Dash-Fontify mode.

\(fn &optional ARG)" t nil)

(autoload 'dash-register-info-lookup "dash" "\
Register the Dash Info manual with `info-lookup-symbol'.
This allows Dash symbols to be looked up with \\[info-lookup-symbol]." t nil)

(register-definition-prefixes "dash" '("!cdr" "!cons" "--" "->" "-a" "-butlast" "-c" "-d" "-e" "-f" "-gr" "-i" "-juxt" "-keep" "-l" "-m" "-no" "-o" "-p" "-r" "-s" "-t" "-u" "-value-to-list" "-when-let" "-zip" "dash-"))

;;;***

;;;### (autoloads nil nil ("dash-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dash-autoloads.el ends here
;;; dash-functional-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dash-functional-autoloads.el ends here
;;; diff-hl-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "diff-hl" "diff-hl.el" (0 0 0 0))
;;; Generated autoloads from diff-hl.el

(autoload 'diff-hl-mode "diff-hl" "\
Toggle VC diff highlighting.

If called interactively, toggle `Diff-Hl mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-diff-hl-mode "diff-hl" "\
Turn on `diff-hl-mode' or `diff-hl-dir-mode' in a buffer if appropriate." nil nil)

(autoload 'diff-hl--global-turn-on "diff-hl" "\
Call `turn-on-diff-hl-mode' if the current major mode is applicable." nil nil)

(autoload 'diff-hl-set-reference-rev "diff-hl" "\
Set the reference revision globally to REV.
When called interactively, REV read with completion.

The default value chosen using one of methods below:

- In a log view buffer, it uses the revision of current entry.
Call `vc-print-log' or `vc-print-root-log' first to open a log
view buffer.
- In a VC annotate buffer, it uses the revision of current line.
- In other situations, it uses the symbol at point.

Notice that this sets the reference revision globally, so in
files from other repositories, `diff-hl-mode' will not highlight
changes correctly, until you run `diff-hl-reset-reference-rev'.

Also notice that this will disable `diff-hl-amend-mode' in
buffers that enables it, since `diff-hl-amend-mode' overrides its
effect.

\(fn REV)" t nil)

(autoload 'diff-hl-reset-reference-rev "diff-hl" "\
Reset the reference revision globally to the most recent one." t nil)

(put 'global-diff-hl-mode 'globalized-minor-mode t)

(defvar global-diff-hl-mode nil "\
Non-nil if Global Diff-Hl mode is enabled.
See the `global-diff-hl-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-diff-hl-mode'.")

(custom-autoload 'global-diff-hl-mode "diff-hl" nil)

(autoload 'global-diff-hl-mode "diff-hl" "\
Toggle Diff-Hl mode in all buffers.
With prefix ARG, enable Global Diff-Hl mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG is
omitted or nil.

Diff-Hl mode is enabled in all buffers where
`diff-hl--global-turn-on' would do it.

See `diff-hl-mode' for more information on Diff-Hl mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "diff-hl" '("diff-hl-"))

;;;***

;;;### (autoloads nil "diff-hl-amend" "diff-hl-amend.el" (0 0 0 0))
;;; Generated autoloads from diff-hl-amend.el

(autoload 'diff-hl-amend-mode "diff-hl-amend" "\
Show changes against the second-last revision in `diff-hl-mode'.
Most useful with backends that support rewriting local commits,
and most importantly, \"amending\" the most recent one.
Currently only supports Git, Mercurial and Bazaar.

If called interactively, toggle `Diff-Hl-Amend mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-diff-hl-amend-mode 'globalized-minor-mode t)

(defvar global-diff-hl-amend-mode nil "\
Non-nil if Global Diff-Hl-Amend mode is enabled.
See the `global-diff-hl-amend-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-diff-hl-amend-mode'.")

(custom-autoload 'global-diff-hl-amend-mode "diff-hl-amend" nil)

(autoload 'global-diff-hl-amend-mode "diff-hl-amend" "\
Toggle Diff-Hl-Amend mode in all buffers.
With prefix ARG, enable Global Diff-Hl-Amend mode if ARG is
positive; otherwise, disable it.  If called from Lisp, enable the mode if ARG
is omitted or nil.

Diff-Hl-Amend mode is enabled in all buffers where
`turn-on-diff-hl-amend-mode' would do it.

See `diff-hl-amend-mode' for more information on Diff-Hl-Amend mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "diff-hl-amend" '("diff-hl-amend-setup" "turn-on-diff-hl-amend-mode"))

;;;***

;;;### (autoloads nil "diff-hl-dired" "diff-hl-dired.el" (0 0 0 0))
;;; Generated autoloads from diff-hl-dired.el

(autoload 'diff-hl-dired-mode "diff-hl-dired" "\
Toggle VC diff highlighting on the side of a Dired window.

If called interactively, toggle `Diff-Hl-Dired mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'diff-hl-dired-mode-unless-remote "diff-hl-dired" nil nil nil)

(register-definition-prefixes "diff-hl-dired" '("diff-hl-dired-"))

;;;***

;;;### (autoloads nil "diff-hl-flydiff" "diff-hl-flydiff.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from diff-hl-flydiff.el

(defvar diff-hl-flydiff-mode nil "\
Non-nil if Diff-Hl-Flydiff mode is enabled.
See the `diff-hl-flydiff-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `diff-hl-flydiff-mode'.")

(custom-autoload 'diff-hl-flydiff-mode "diff-hl-flydiff" nil)

(autoload 'diff-hl-flydiff-mode "diff-hl-flydiff" "\
Perform highlighting on-the-fly.
This is a global minor mode.  It alters how `diff-hl-mode' works.

If called interactively, toggle `Diff-Hl-Flydiff mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "diff-hl-flydiff" '("diff-hl-flydiff"))

;;;***

;;;### (autoloads nil "diff-hl-inline-popup" "diff-hl-inline-popup.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from diff-hl-inline-popup.el

(autoload 'diff-hl-inline-popup-hide "diff-hl-inline-popup" "\
Hide the current inline popup." t nil)

(autoload 'diff-hl-inline-popup-show "diff-hl-inline-popup" "\
Create a phantom overlay to show the inline popup, with some
content LINES, and a HEADER and a FOOTER, at POINT.  KEYMAP is
added to the current keymaps.  CLOSE-HOOK is called when the popup
is closed.

\(fn LINES &optional HEADER FOOTER KEYMAP CLOSE-HOOK POINT HEIGHT)" nil nil)

(register-definition-prefixes "diff-hl-inline-popup" '("diff-hl-inline-popup-"))

;;;***

;;;### (autoloads nil "diff-hl-margin" "diff-hl-margin.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from diff-hl-margin.el

(defvar diff-hl-margin-mode nil "\
Non-nil if Diff-Hl-Margin mode is enabled.
See the `diff-hl-margin-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `diff-hl-margin-mode'.")

(custom-autoload 'diff-hl-margin-mode "diff-hl-margin" nil)

(autoload 'diff-hl-margin-mode "diff-hl-margin" "\
Toggle displaying `diff-hl-mode' highlights on the margin.

If called interactively, toggle `Diff-Hl-Margin mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'diff-hl-margin-local-mode "diff-hl-margin" "\
Toggle displaying `diff-hl-mode' highlights on the margin locally.
You probably shouldn't use this function directly.

If called interactively, toggle `Diff-Hl-Margin-Local mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "diff-hl-margin" '("diff-hl-"))

;;;***

;;;### (autoloads nil "diff-hl-show-hunk" "diff-hl-show-hunk.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from diff-hl-show-hunk.el

(autoload 'diff-hl-show-hunk-inline-popup "diff-hl-show-hunk" "\
Implementation to show the hunk in a inline popup.
BUFFER is a buffer with the hunk.

\(fn BUFFER &optional IGNORED-LINE)" nil nil)

(autoload 'diff-hl-show-hunk-previous "diff-hl-show-hunk" "\
Go to previous hunk/change and show it." t nil)

(autoload 'diff-hl-show-hunk-next "diff-hl-show-hunk" "\
Go to next hunk/change and show it." t nil)

(autoload 'diff-hl-show-hunk "diff-hl-show-hunk" "\
Show the VC diff hunk at point.
The backend is determined by `diff-hl-show-hunk-function'." t nil)

(autoload 'diff-hl-show-hunk-mouse-mode "diff-hl-show-hunk" "\
Enables the margin and fringe to show a posframe/popup with vc diffs when clicked.
By default, the popup shows only the current hunk, and
the line of the hunk that matches the current position is
highlighted.  The face, border and other visual preferences are
customizable.  It can be also invoked with the command
`diff-hl-show-hunk'
\\{diff-hl-show-hunk-mouse-mode-map}

If called interactively, toggle `Diff-Hl-Show-Hunk-Mouse mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-diff-hl-show-hunk-mouse-mode 'globalized-minor-mode t)

(defvar global-diff-hl-show-hunk-mouse-mode nil "\
Non-nil if Global Diff-Hl-Show-Hunk-Mouse mode is enabled.
See the `global-diff-hl-show-hunk-mouse-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-diff-hl-show-hunk-mouse-mode'.")

(custom-autoload 'global-diff-hl-show-hunk-mouse-mode "diff-hl-show-hunk" nil)

(autoload 'global-diff-hl-show-hunk-mouse-mode "diff-hl-show-hunk" "\
Toggle Diff-Hl-Show-Hunk-Mouse mode in all buffers.
With prefix ARG, enable Global Diff-Hl-Show-Hunk-Mouse mode if ARG
is positive; otherwise, disable it.  If called from Lisp, enable the
mode if ARG is omitted or nil.

Diff-Hl-Show-Hunk-Mouse mode is enabled in all buffers where
`diff-hl-show-hunk-mouse-mode' would do it.

See `diff-hl-show-hunk-mouse-mode' for more information on
Diff-Hl-Show-Hunk-Mouse mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "diff-hl-show-hunk" '("diff-hl-show-hunk-"))

;;;***

;;;### (autoloads nil "diff-hl-show-hunk-posframe" "diff-hl-show-hunk-posframe.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from diff-hl-show-hunk-posframe.el

(autoload 'diff-hl-show-hunk-posframe "diff-hl-show-hunk-posframe" "\
Implementation to show the hunk in a posframe.

\(fn BUFFER &optional LINE)" nil nil)

(register-definition-prefixes "diff-hl-show-hunk-posframe" '("diff-hl-show-hunk-"))

;;;***

;;;### (autoloads nil nil ("diff-hl-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; diff-hl-autoloads.el ends here
;;; doom-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "doom" "doom.el" (0 0 0 0))
;;; Generated autoloads from doom.el

(register-definition-prefixes "doom" '("doom-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; doom-autoloads.el ends here
;;; doom-modeline-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "doom-modeline" "doom-modeline.el" (0 0 0 0))
;;; Generated autoloads from doom-modeline.el

(autoload 'doom-modeline-init "doom-modeline" "\
Initialize doom mode-line." nil nil)

(autoload 'doom-modeline-set-main-modeline "doom-modeline" "\
Set main mode-line.
If DEFAULT is non-nil, set the default mode-line for all buffers.

\(fn &optional DEFAULT)" nil nil)

(autoload 'doom-modeline-set-minimal-modeline "doom-modeline" "\
Set minimal mode-line." nil nil)

(autoload 'doom-modeline-set-special-modeline "doom-modeline" "\
Set special mode-line." nil nil)

(autoload 'doom-modeline-set-project-modeline "doom-modeline" "\
Set project mode-line." nil nil)

(autoload 'doom-modeline-set-dashboard-modeline "doom-modeline" "\
Set dashboard mode-line." nil nil)

(autoload 'doom-modeline-set-vcs-modeline "doom-modeline" "\
Set vcs mode-line." nil nil)

(autoload 'doom-modeline-set-info-modeline "doom-modeline" "\
Set Info mode-line." nil nil)

(autoload 'doom-modeline-set-package-modeline "doom-modeline" "\
Set package mode-line." nil nil)

(autoload 'doom-modeline-set-media-modeline "doom-modeline" "\
Set media mode-line." nil nil)

(autoload 'doom-modeline-set-message-modeline "doom-modeline" "\
Set message mode-line." nil nil)

(autoload 'doom-modeline-set-pdf-modeline "doom-modeline" "\
Set pdf mode-line." nil nil)

(autoload 'doom-modeline-set-org-src-modeline "doom-modeline" "\
Set org-src mode-line." nil nil)

(autoload 'doom-modeline-set-helm-modeline "doom-modeline" "\
Set helm mode-line.

\(fn &rest _)" nil nil)

(autoload 'doom-modeline-set-timemachine-modeline "doom-modeline" "\
Set timemachine mode-line." nil nil)

(defvar doom-modeline-mode nil "\
Non-nil if Doom-Modeline mode is enabled.
See the `doom-modeline-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `doom-modeline-mode'.")

(custom-autoload 'doom-modeline-mode "doom-modeline" nil)

(autoload 'doom-modeline-mode "doom-modeline" "\
Toggle doom-modeline on or off.

If called interactively, toggle `Doom-Modeline mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "doom-modeline" '("doom-modeline-"))

;;;***

;;;### (autoloads nil "doom-modeline-core" "doom-modeline-core.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-modeline-core.el

(register-definition-prefixes "doom-modeline-core" '("doom-modeline"))

;;;***

;;;### (autoloads nil "doom-modeline-env" "doom-modeline-env.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-modeline-env.el
 (autoload 'doom-modeline-env-setup-python "doom-modeline-env")
 (autoload 'doom-modeline-env-setup-ruby "doom-modeline-env")
 (autoload 'doom-modeline-env-setup-perl "doom-modeline-env")
 (autoload 'doom-modeline-env-setup-go "doom-modeline-env")
 (autoload 'doom-modeline-env-setup-elixir "doom-modeline-env")
 (autoload 'doom-modeline-env-setup-rust "doom-modeline-env")

(register-definition-prefixes "doom-modeline-env" '("doom-modeline-"))

;;;***

;;;### (autoloads nil "doom-modeline-segments" "doom-modeline-segments.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-modeline-segments.el

(register-definition-prefixes "doom-modeline-segments" '("doom-modeline-"))

;;;***

;;;### (autoloads nil nil ("doom-modeline-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; doom-modeline-autoloads.el ends here
;;; doom-themes-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "doom-Iosvkem-theme" "doom-Iosvkem-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-Iosvkem-theme.el

(register-definition-prefixes "doom-Iosvkem-theme" '("doom-Iosvkem"))

;;;***

;;;### (autoloads nil "doom-acario-dark-theme" "doom-acario-dark-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-acario-dark-theme.el

(register-definition-prefixes "doom-acario-dark-theme" '("doom-acario-dark"))

;;;***

;;;### (autoloads nil "doom-acario-light-theme" "doom-acario-light-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-acario-light-theme.el

(register-definition-prefixes "doom-acario-light-theme" '("doom-acario-light"))

;;;***

;;;### (autoloads nil "doom-ayu-light-theme" "doom-ayu-light-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-ayu-light-theme.el

(register-definition-prefixes "doom-ayu-light-theme" '("doom-ayu-light"))

;;;***

;;;### (autoloads nil "doom-ayu-mirage-theme" "doom-ayu-mirage-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-ayu-mirage-theme.el

(register-definition-prefixes "doom-ayu-mirage-theme" '("doom-ayu-mirage"))

;;;***

;;;### (autoloads nil "doom-challenger-deep-theme" "doom-challenger-deep-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-challenger-deep-theme.el

(register-definition-prefixes "doom-challenger-deep-theme" '("doom-challenger-deep"))

;;;***

;;;### (autoloads nil "doom-city-lights-theme" "doom-city-lights-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-city-lights-theme.el

(register-definition-prefixes "doom-city-lights-theme" '("doom-city-lights"))

;;;***

;;;### (autoloads nil "doom-dark+-theme" "doom-dark+-theme.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from doom-dark+-theme.el

(register-definition-prefixes "doom-dark+-theme" '("doom-dark+"))

;;;***

;;;### (autoloads nil "doom-dracula-theme" "doom-dracula-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-dracula-theme.el

(register-definition-prefixes "doom-dracula-theme" '("doom-dracula"))

;;;***

;;;### (autoloads nil "doom-ephemeral-theme" "doom-ephemeral-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-ephemeral-theme.el

(register-definition-prefixes "doom-ephemeral-theme" '("doom-ephemeral"))

;;;***

;;;### (autoloads nil "doom-fairy-floss-theme" "doom-fairy-floss-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-fairy-floss-theme.el

(register-definition-prefixes "doom-fairy-floss-theme" '("doom-fairy-floss"))

;;;***

;;;### (autoloads nil "doom-flatwhite-theme" "doom-flatwhite-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-flatwhite-theme.el

(register-definition-prefixes "doom-flatwhite-theme" '("doom-f"))

;;;***

;;;### (autoloads nil "doom-gruvbox-light-theme" "doom-gruvbox-light-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-gruvbox-light-theme.el

(register-definition-prefixes "doom-gruvbox-light-theme" '("doom-gruvbox-light"))

;;;***

;;;### (autoloads nil "doom-gruvbox-theme" "doom-gruvbox-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-gruvbox-theme.el

(register-definition-prefixes "doom-gruvbox-theme" '("doom-gruvbox"))

;;;***

;;;### (autoloads nil "doom-henna-theme" "doom-henna-theme.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from doom-henna-theme.el

(register-definition-prefixes "doom-henna-theme" '("doom-henna"))

;;;***

;;;### (autoloads nil "doom-homage-black-theme" "doom-homage-black-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-homage-black-theme.el

(register-definition-prefixes "doom-homage-black-theme" '("doom-homage-black"))

;;;***

;;;### (autoloads nil "doom-homage-white-theme" "doom-homage-white-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-homage-white-theme.el

(register-definition-prefixes "doom-homage-white-theme" '("doom-homage-white"))

;;;***

;;;### (autoloads nil "doom-horizon-theme" "doom-horizon-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-horizon-theme.el

(register-definition-prefixes "doom-horizon-theme" '("doom-horizon"))

;;;***

;;;### (autoloads nil "doom-laserwave-theme" "doom-laserwave-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-laserwave-theme.el

(register-definition-prefixes "doom-laserwave-theme" '("doom-laserwave"))

;;;***

;;;### (autoloads nil "doom-manegarm-theme" "doom-manegarm-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-manegarm-theme.el

(register-definition-prefixes "doom-manegarm-theme" '("doom-manegarm"))

;;;***

;;;### (autoloads nil "doom-material-theme" "doom-material-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-material-theme.el

(register-definition-prefixes "doom-material-theme" '("doom-material"))

;;;***

;;;### (autoloads nil "doom-miramare-theme" "doom-miramare-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-miramare-theme.el

(register-definition-prefixes "doom-miramare-theme" '("doom-miramare"))

;;;***

;;;### (autoloads nil "doom-molokai-theme" "doom-molokai-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-molokai-theme.el

(register-definition-prefixes "doom-molokai-theme" '("doom-molokai"))

;;;***

;;;### (autoloads nil "doom-monokai-classic-theme" "doom-monokai-classic-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-monokai-classic-theme.el

(register-definition-prefixes "doom-monokai-classic-theme" '("doom-monokai-classic"))

;;;***

;;;### (autoloads nil "doom-monokai-pro-theme" "doom-monokai-pro-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-monokai-pro-theme.el

(register-definition-prefixes "doom-monokai-pro-theme" '("doom-monokai-pro"))

;;;***

;;;### (autoloads nil "doom-monokai-spectrum-theme" "doom-monokai-spectrum-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-monokai-spectrum-theme.el

(register-definition-prefixes "doom-monokai-spectrum-theme" '("doom-monokai-spectrum"))

;;;***

;;;### (autoloads nil "doom-moonlight-theme" "doom-moonlight-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-moonlight-theme.el

(register-definition-prefixes "doom-moonlight-theme" '("doom-moonlight"))

;;;***

;;;### (autoloads nil "doom-nord-light-theme" "doom-nord-light-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-nord-light-theme.el

(register-definition-prefixes "doom-nord-light-theme" '("doom-nord-light"))

;;;***

;;;### (autoloads nil "doom-nord-theme" "doom-nord-theme.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from doom-nord-theme.el

(register-definition-prefixes "doom-nord-theme" '("doom-nord"))

;;;***

;;;### (autoloads nil "doom-nova-theme" "doom-nova-theme.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from doom-nova-theme.el

(register-definition-prefixes "doom-nova-theme" '("doom-nova"))

;;;***

;;;### (autoloads nil "doom-oceanic-next-theme" "doom-oceanic-next-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-oceanic-next-theme.el

(register-definition-prefixes "doom-oceanic-next-theme" '("doom-oceanic-next"))

;;;***

;;;### (autoloads nil "doom-old-hope-theme" "doom-old-hope-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-old-hope-theme.el

(register-definition-prefixes "doom-old-hope-theme" '("doom-old-hope"))

;;;***

;;;### (autoloads nil "doom-one-light-theme" "doom-one-light-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-one-light-theme.el

(register-definition-prefixes "doom-one-light-theme" '("doom-one-light"))

;;;***

;;;### (autoloads nil "doom-one-theme" "doom-one-theme.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from doom-one-theme.el

(register-definition-prefixes "doom-one-theme" '("doom-one"))

;;;***

;;;### (autoloads nil "doom-opera-light-theme" "doom-opera-light-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-opera-light-theme.el

(register-definition-prefixes "doom-opera-light-theme" '("doom-opera-light"))

;;;***

;;;### (autoloads nil "doom-opera-theme" "doom-opera-theme.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from doom-opera-theme.el

(register-definition-prefixes "doom-opera-theme" '("doom-opera"))

;;;***

;;;### (autoloads nil "doom-outrun-electric-theme" "doom-outrun-electric-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-outrun-electric-theme.el

(register-definition-prefixes "doom-outrun-electric-theme" '("doom-outrun-electric"))

;;;***

;;;### (autoloads nil "doom-palenight-theme" "doom-palenight-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-palenight-theme.el

(register-definition-prefixes "doom-palenight-theme" '("doom-palenight"))

;;;***

;;;### (autoloads nil "doom-peacock-theme" "doom-peacock-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-peacock-theme.el

(register-definition-prefixes "doom-peacock-theme" '("doom-peacock"))

;;;***

;;;### (autoloads nil "doom-plain-dark-theme" "doom-plain-dark-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-plain-dark-theme.el

(register-definition-prefixes "doom-plain-dark-theme" '("doom-plain-"))

;;;***

;;;### (autoloads nil "doom-plain-theme" "doom-plain-theme.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from doom-plain-theme.el

(register-definition-prefixes "doom-plain-theme" '("doom-plain"))

;;;***

;;;### (autoloads nil "doom-rouge-theme" "doom-rouge-theme.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from doom-rouge-theme.el

(register-definition-prefixes "doom-rouge-theme" '("doom-rouge"))

;;;***

;;;### (autoloads nil "doom-snazzy-theme" "doom-snazzy-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-snazzy-theme.el

(register-definition-prefixes "doom-snazzy-theme" '("doom-snazzy"))

;;;***

;;;### (autoloads nil "doom-solarized-dark-theme" "doom-solarized-dark-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-solarized-dark-theme.el

(register-definition-prefixes "doom-solarized-dark-theme" '("doom-solarized-dark"))

;;;***

;;;### (autoloads nil "doom-solarized-light-theme" "doom-solarized-light-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-solarized-light-theme.el

(register-definition-prefixes "doom-solarized-light-theme" '("doom-solarized-light"))

;;;***

;;;### (autoloads nil "doom-sourcerer-theme" "doom-sourcerer-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-sourcerer-theme.el

(register-definition-prefixes "doom-sourcerer-theme" '("doom-sourcerer"))

;;;***

;;;### (autoloads nil "doom-spacegrey-theme" "doom-spacegrey-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-spacegrey-theme.el

(register-definition-prefixes "doom-spacegrey-theme" '("doom-spacegrey"))

;;;***

;;;### (autoloads nil "doom-themes" "doom-themes.el" (0 0 0 0))
;;; Generated autoloads from doom-themes.el

(autoload 'doom-name-to-rgb "doom-themes" "\
Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame).

\(fn COLOR)" nil nil)

(autoload 'doom-blend "doom-themes" "\
Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)

\(fn COLOR1 COLOR2 ALPHA)" nil nil)

(autoload 'doom-darken "doom-themes" "\
Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1).

\(fn COLOR ALPHA)" nil nil)

(autoload 'doom-lighten "doom-themes" "\
Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1).

\(fn COLOR ALPHA)" nil nil)

(autoload 'doom-color "doom-themes" "\
Retrieve a specific color named NAME (a symbol) from the current theme.

\(fn NAME &optional TYPE)" nil nil)

(autoload 'doom-ref "doom-themes" "\
TODO

\(fn FACE PROP &optional CLASS)" nil nil)

(autoload 'doom-themes-set-faces "doom-themes" "\
Customize THEME (a symbol) with FACES.

If THEME is nil, it applies to all themes you load. FACES is a list of Doom
theme face specs. These is a simplified spec. For example:

  (doom-themes-set-faces 'user
    '(default :background red :foreground blue)
    '(doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
    '(doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
    '(doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
    '(doom-modeline-buffer-project-root :foreground green :weight 'bold))

\(fn THEME &rest FACES)" nil nil)

(function-put 'doom-themes-set-faces 'lisp-indent-function 'defun)

(when (and (boundp 'custom-theme-load-path) load-file-name) (let* ((base (file-name-directory load-file-name)) (dir (expand-file-name "themes/" base))) (add-to-list 'custom-theme-load-path (or (and (file-directory-p dir) dir) base))))

(register-definition-prefixes "doom-themes" '("def-doom-theme" "doom-"))

;;;***

;;;### (autoloads nil "doom-themes-base" "doom-themes-base.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from doom-themes-base.el

(register-definition-prefixes "doom-themes-base" '("doom-themes-base-"))

;;;***

;;;### (autoloads nil "doom-themes-ext-neotree" "doom-themes-ext-neotree.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-themes-ext-neotree.el

(autoload 'doom-themes-neotree-config "doom-themes-ext-neotree" "\
Install doom-themes' neotree configuration.

Includes an Atom-esque icon theme and highlighting based on filetype." nil nil)

(register-definition-prefixes "doom-themes-ext-neotree" '("doom-"))

;;;***

;;;### (autoloads nil "doom-themes-ext-org" "doom-themes-ext-org.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-themes-ext-org.el

(autoload 'doom-themes-org-config "doom-themes-ext-org" "\
Load `doom-themes-ext-org'." nil nil)

(register-definition-prefixes "doom-themes-ext-org" '("doom-themes-"))

;;;***

;;;### (autoloads nil "doom-themes-ext-treemacs" "doom-themes-ext-treemacs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-themes-ext-treemacs.el

(autoload 'doom-themes-treemacs-config "doom-themes-ext-treemacs" "\
Install doom-themes' treemacs configuration.

Includes an Atom-esque icon theme and highlighting based on filetype." nil nil)

(register-definition-prefixes "doom-themes-ext-treemacs" '("doom-themes-"))

;;;***

;;;### (autoloads nil "doom-themes-ext-visual-bell" "doom-themes-ext-visual-bell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-themes-ext-visual-bell.el

(autoload 'doom-themes-visual-bell-fn "doom-themes-ext-visual-bell" "\
Blink the mode-line red briefly. Set `ring-bell-function' to this to use it." nil nil)

(autoload 'doom-themes-visual-bell-config "doom-themes-ext-visual-bell" "\
Enable flashing the mode-line on error." nil nil)

;;;***

;;;### (autoloads nil "doom-tomorrow-day-theme" "doom-tomorrow-day-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-tomorrow-day-theme.el

(register-definition-prefixes "doom-tomorrow-day-theme" '("doom-tomorrow-day"))

;;;***

;;;### (autoloads nil "doom-tomorrow-night-theme" "doom-tomorrow-night-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-tomorrow-night-theme.el

(register-definition-prefixes "doom-tomorrow-night-theme" '("doom-tomorrow-night"))

;;;***

;;;### (autoloads nil "doom-vibrant-theme" "doom-vibrant-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-vibrant-theme.el

(register-definition-prefixes "doom-vibrant-theme" '("doom-vibrant"))

;;;***

;;;### (autoloads nil "doom-wilmersdorf-theme" "doom-wilmersdorf-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-wilmersdorf-theme.el

(register-definition-prefixes "doom-wilmersdorf-theme" '("doom-wilmersdorf"))

;;;***

;;;### (autoloads nil "doom-zenburn-theme" "doom-zenburn-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-zenburn-theme.el

(register-definition-prefixes "doom-zenburn-theme" '("doom-zenburn"))

;;;***

;;;### (autoloads nil nil ("doom-themes-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; doom-themes-autoloads.el ends here
;;; dumb-jump-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dumb-jump" "dumb-jump.el" (0 0 0 0))
;;; Generated autoloads from dumb-jump.el

(defvar dumb-jump-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "C-M-g") 'dumb-jump-go) (define-key map (kbd "C-M-p") 'dumb-jump-back) (define-key map (kbd "C-M-q") 'dumb-jump-quick-look) map))

(autoload 'dumb-jump-back "dumb-jump" "\
Jump back to where the last jump was done." t nil)

(autoload 'dumb-jump-quick-look "dumb-jump" "\
Run dumb-jump-go in quick look mode.  That is, show a tooltip of where it would jump instead." t nil)

(autoload 'dumb-jump-go-other-window "dumb-jump" "\
Like 'dumb-jump-go' but use 'find-file-other-window' instead of 'find-file'." t nil)

(autoload 'dumb-jump-go-current-window "dumb-jump" "\
Like dumb-jump-go but always use 'find-file'." t nil)

(autoload 'dumb-jump-go-prefer-external "dumb-jump" "\
Like dumb-jump-go but prefer external matches from the current file." t nil)

(autoload 'dumb-jump-go-prompt "dumb-jump" "\
Like dumb-jump-go but prompts for function instead of using under point" t nil)

(autoload 'dumb-jump-go-prefer-external-other-window "dumb-jump" "\
Like dumb-jump-go-prefer-external but use 'find-file-other-window' instead of 'find-file'." t nil)

(autoload 'dumb-jump-go "dumb-jump" "\
Go to the function/variable declaration for thing at point.
When USE-TOOLTIP is t a tooltip jump preview will show instead.
When PREFER-EXTERNAL is t it will sort external matches before
current file.

\(fn &optional USE-TOOLTIP PREFER-EXTERNAL PROMPT)" t nil)

(defvar dumb-jump-mode nil "\
Non-nil if Dumb-Jump mode is enabled.
See the `dumb-jump-mode' command
for a description of this minor mode.")

(custom-autoload 'dumb-jump-mode "dumb-jump" nil)

(autoload 'dumb-jump-mode "dumb-jump" "\
Minor mode for jumping to variable and function definitions

If called interactively, enable Dumb-Jump mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'dumb-jump-xref-activate "dumb-jump" "\
Function to activate xref backend.
Add this function to `xref-backend-functions' to dumb jump to be
activiated, whenever it finds a project. It is recommended to add
it to the end, so that it only gets activated when no better
option is found." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dumb-jump" '("dumb-jump-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dumb-jump-autoloads.el ends here
;;; edit-indirect-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "edit-indirect" "edit-indirect.el" (0 0 0 0))
;;; Generated autoloads from edit-indirect.el

(autoload 'edit-indirect-region "edit-indirect" "\
Edit the region BEG..END in a separate buffer.
The region is copied, without text properties, to a separate
buffer, called edit-indirect buffer, and
`edit-indirect-guess-mode-function' is called to set the major
mode.
When done, exit with `edit-indirect-commit', which will remove the
original region and replace it with the edited version; or with
`edit-indirect-abort', which will drop the modifications.

This differs from `clone-indirect-buffer' with narrowing in that
the text properties are not shared, so the parent buffer major mode
and the edit-indirect buffer major mode will not be able to tread
on each other's toes by setting up potentially conflicting text
properties, which happens surprisingly often when the font-lock
mode is used.

Edit-indirect buffers use the `edit-indirect-mode-map' keymap.

If there's already an edit-indirect buffer for BEG..END, use that.
If there's already an edit-indirect buffer active overlapping any
portion of BEG..END, an `edit-indirect-overlapping' error is
signaled.

When DISPLAY-BUFFER is non-nil or when called interactively,
display the edit-indirect buffer in some window and select it.

In any case, return the edit-indirect buffer.

\(fn BEG END &optional DISPLAY-BUFFER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "edit-indirect" '("edit-indirect-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; edit-indirect-autoloads.el ends here
;;; elpy-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elpy" "elpy.el" (0 0 0 0))
;;; Generated autoloads from elpy.el

(autoload 'elpy-enable "elpy" "\
Enable Elpy in all future Python buffers.

\(fn &optional IGNORED)" t nil)

(autoload 'elpy-mode "elpy" "\
Minor mode in Python buffers for the Emacs Lisp Python Environment.

If called interactively, toggle `Elpy mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This mode fully supports virtualenvs. Once you switch a
virtualenv using \\[pyvenv-workon], you can use
\\[elpy-rpc-restart] to make the elpy Python process use your
virtualenv.

\\{elpy-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'elpy-config "elpy" "\
Configure Elpy.

This function will pop up a configuration buffer, which is mostly
a customize buffer, but has some more options." t nil)

(autoload 'elpy-version "elpy" "\
Display the version of Elpy." t nil)

(register-definition-prefixes "elpy" '("elpy-"))

;;;***

;;;### (autoloads nil "elpy-django" "elpy-django.el" (0 0 0 0))
;;; Generated autoloads from elpy-django.el

(register-definition-prefixes "elpy-django" '("elpy-"))

;;;***

;;;### (autoloads nil "elpy-profile" "elpy-profile.el" (0 0 0 0))
;;; Generated autoloads from elpy-profile.el

(register-definition-prefixes "elpy-profile" '("elpy-profile-"))

;;;***

;;;### (autoloads nil "elpy-refactor" "elpy-refactor.el" (0 0 0 0))
;;; Generated autoloads from elpy-refactor.el

(register-definition-prefixes "elpy-refactor" '("elpy-refactor-"))

;;;***

;;;### (autoloads nil "elpy-rpc" "elpy-rpc.el" (0 0 0 0))
;;; Generated autoloads from elpy-rpc.el

(register-definition-prefixes "elpy-rpc" '("elpy-" "with-elpy-rpc-virtualenv-activated"))

;;;***

;;;### (autoloads nil "elpy-shell" "elpy-shell.el" (0 0 0 0))
;;; Generated autoloads from elpy-shell.el

(register-definition-prefixes "elpy-shell" '("elpy-"))

;;;***

;;;### (autoloads nil nil ("elpy-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elpy-autoloads.el ends here
;;; embark-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "embark" "embark.el" (0 0 0 0))
;;; Generated autoloads from embark.el

(defun embark--record-this-command nil "\
Record command which opened the minibuffer.
We record this because it will be the default action.
This function is meant to be added to `minibuffer-setup-hook'." (setq-local embark--command this-command))

(add-hook 'minibuffer-setup-hook #'embark--record-this-command)

(autoload 'embark-prefix-help-command "embark" "\
Prompt for and run a command bound in the prefix used to reach this command.
The prefix described consists of all but the last event of the
key sequence that ran this command.  This function is intended to
be used as a value for `prefix-help-command'.

In addition to using completion to select a command, you can also
type @ and the key binding (without the prefix)." t nil)

(autoload 'embark-bindings "embark" "\
Explore all current command key bindings with `completing-read'.
The selected command will be executed.  The set of key bindings can
be restricted by passing a PREFIX key.

\(fn &optional PREFIX)" t nil)

(autoload 'embark-act "embark" "\
Prompt the user for an action and perform it.
The target of the action is chosen by `embark-target-finders'.
By default, if called from a minibuffer the target is the top
completion candidate, if called from an Embark Collect or a
Completions buffer it is the candidate at point.

This command uses `embark-prompter' to ask the user to specify an
action, and calls it injecting the target at the first minibuffer
prompt.

If you call this from the minibuffer, it can optionally quit the
minibuffer.  The variable `embark-quit-after-action' controls
whether calling `embark-act' with nil ARG quits the minibuffer,
and if ARG is non-nil it will do the opposite.  Interactively,
ARG is the prefix argument.

\(fn &optional ARG)" t nil)

(autoload 'embark-default-action "embark" "\
Run the default action on the current target.
The target of the action is chosen by `embark-target-finders'.

If the target comes from minibuffer completion, then the default
action is the command that opened the minibuffer in the first
place, unless overidden by `embark-default-action-overrides'.

For targets that do not come from minibuffer completion
\(typically some thing at point in a regular buffer) and whose
type is not listed in `embark-default-action-overrides', the
default action is given by whatever binding RET has in the action
keymap for the target's type." t nil)

(autoload 'embark-become "embark" "\
Make current command become a different command.
Take the current minibuffer input as initial input for new
command.  The new command can be run normally using key bindings or
\\[execute-extended-command], but if the current command is found in a keymap in
`embark-become-keymaps', that keymap is activated to provide
convenient access to the other commands in it.

If FULL is non-nil (interactively, if called with a prefix
argument), the entire minibuffer contents are used as the initial
input of the new command.  By default only the part of the
minibuffer contents between the current completion boundaries is
taken.  What this means is fairly technical, but (1) usually
there is no difference: the completion boundaries include the
entire minibuffer contents, and (2) the most common case where
these notions differ is file completion, in which case the
completion boundaries single out the path component containing
point.

\(fn &optional FULL)" t nil)

(autoload 'embark-collect-live "embark" "\
Create a live-updating Embark Collect buffer.
Optionally start in INITIAL-VIEW (either `list' or `grid')
instead of what `embark-collect-initial-view-alist' specifies.
Interactively, \\[universal-argument] means grid view, a prefix
argument of 1 means list view.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Collect Live\".

\(fn &optional INITIAL-VIEW)" t nil)

(autoload 'embark-collect-snapshot "embark" "\
Create an Embark Collect buffer.
Optionally start in INITIAL-VIEW (either `list' or `grid')
instead of what `embark-collect-initial-view-alist' specifies.
Interactively, \\[universal-argument] means grid view, a prefix
argument of 1 means list view.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Collect\".

\(fn &optional INITIAL-VIEW)" t nil)

(autoload 'embark-collect-completions "embark" "\
Create an ephemeral live-updating Embark Collect buffer." t nil)

(autoload 'embark-collect-completions-after-delay "embark" "\
Start `embark-collect-live' after `embark-collect-live-initial-delay'.
Add this function to `minibuffer-setup-hook' to have an Embark
Live Collect buffer popup every time you use the minibuffer." nil nil)

(autoload 'embark-collect-completions-after-input "embark" "\
Start `embark-collect-completions' after some minibuffer input.
Add this function to `minibuffer-setup-hook' to have an Embark
Live Collect buffer popup soon after you type something in the
minibuffer; the length of the delay after typing is given by
`embark-collect-live-initial-delay'." nil nil)

(autoload 'embark-switch-to-collect-completions "embark" "\
Switch to the Embark Collect Completions buffer, creating it if necessary." t nil)

(autoload 'embark-export "embark" "\
Create a type-specific buffer to manage current candidates.
The variable `embark-exporters-alist' controls how to make the
buffer for each type of completion." t nil)

(register-definition-prefixes "embark" '("embark-"))

;;;***

;;;### (autoloads nil nil ("embark-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; embark-autoloads.el ends here
;;; epl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "epl" "epl.el" (0 0 0 0))
;;; Generated autoloads from epl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epl" '("epl-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; epl-autoloads.el ends here
;;; eshell-z-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eshell-z" "eshell-z.el" (0 0 0 0))
;;; Generated autoloads from eshell-z.el

(autoload 'eshell-z "eshell-z" "\
Switch to eshell and change directory to DIR.

\(fn DIR)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eshell-z" '("eshell" "pcomplete/z")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eshell-z-autoloads.el ends here
;;; esup-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "esup" "esup.el" (0 0 0 0))
;;; Generated autoloads from esup.el

(autoload 'esup "esup" "\
Profile the startup time of Emacs in the background.
If INIT-FILE is non-nil, profile that instead of USER-INIT-FILE.
ARGS is a list of extra command line arguments to pass to Emacs.

\(fn &optional INIT-FILE &rest ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "esup" '("esup-")))

;;;***

;;;### (autoloads nil "esup-child" "esup-child.el" (0 0 0 0))
;;; Generated autoloads from esup-child.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "esup-child" '("esup-" "with-esup-child-increasing-depth")))

;;;***

;;;### (autoloads nil nil ("esup-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; esup-autoloads.el ends here
;;; evil-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-command-window" "evil-command-window.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-command-window.el

(register-definition-prefixes "evil-command-window" '("evil-"))

;;;***

;;;### (autoloads nil "evil-commands" "evil-commands.el" (0 0 0 0))
;;; Generated autoloads from evil-commands.el

(register-definition-prefixes "evil-commands" '("evil-"))

;;;***

;;;### (autoloads nil "evil-common" "evil-common.el" (0 0 0 0))
;;; Generated autoloads from evil-common.el

(register-definition-prefixes "evil-common" '("bounds-of-evil-" "evil-" "forward-evil-"))

;;;***

;;;### (autoloads nil "evil-core" "evil-core.el" (0 0 0 0))
;;; Generated autoloads from evil-core.el
 (autoload 'evil-mode "evil" nil t)

(register-definition-prefixes "evil-core" '("evil-" "turn-o"))

;;;***

;;;### (autoloads nil "evil-digraphs" "evil-digraphs.el" (0 0 0 0))
;;; Generated autoloads from evil-digraphs.el

(register-definition-prefixes "evil-digraphs" '("evil-digraph"))

;;;***

;;;### (autoloads nil "evil-ex" "evil-ex.el" (0 0 0 0))
;;; Generated autoloads from evil-ex.el

(register-definition-prefixes "evil-ex" '("evil-"))

;;;***

;;;### (autoloads nil "evil-integration" "evil-integration.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from evil-integration.el

(register-definition-prefixes "evil-integration" '("evil-"))

;;;***

;;;### (autoloads nil "evil-jumps" "evil-jumps.el" (0 0 0 0))
;;; Generated autoloads from evil-jumps.el

(register-definition-prefixes "evil-jumps" '("evil-"))

;;;***

;;;### (autoloads nil "evil-macros" "evil-macros.el" (0 0 0 0))
;;; Generated autoloads from evil-macros.el

(register-definition-prefixes "evil-macros" '("evil-"))

;;;***

;;;### (autoloads nil "evil-maps" "evil-maps.el" (0 0 0 0))
;;; Generated autoloads from evil-maps.el

(register-definition-prefixes "evil-maps" '("evil-"))

;;;***

;;;### (autoloads nil "evil-repeat" "evil-repeat.el" (0 0 0 0))
;;; Generated autoloads from evil-repeat.el

(register-definition-prefixes "evil-repeat" '("evil-"))

;;;***

;;;### (autoloads nil "evil-search" "evil-search.el" (0 0 0 0))
;;; Generated autoloads from evil-search.el

(register-definition-prefixes "evil-search" '("evil-"))

;;;***

;;;### (autoloads nil "evil-states" "evil-states.el" (0 0 0 0))
;;; Generated autoloads from evil-states.el

(register-definition-prefixes "evil-states" '("evil-"))

;;;***

;;;### (autoloads nil "evil-types" "evil-types.el" (0 0 0 0))
;;; Generated autoloads from evil-types.el

(register-definition-prefixes "evil-types" '("evil-ex-get-optional-register-and-count"))

;;;***

;;;### (autoloads nil "evil-vars" "evil-vars.el" (0 0 0 0))
;;; Generated autoloads from evil-vars.el

(register-definition-prefixes "evil-vars" '("evil-"))

;;;***

;;;### (autoloads nil nil ("evil-development.el" "evil-keybindings.el"
;;;;;;  "evil-pkg.el" "evil.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-autoloads.el ends here
;;; evil-avy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-avy" "evil-avy.el" (0 0 0 0))
;;; Generated autoloads from evil-avy.el

(defvar evil-avy-mode nil "\
Non-nil if Evil-Avy mode is enabled.
See the `evil-avy-mode' command
for a description of this minor mode.")

(custom-autoload 'evil-avy-mode "evil-avy" nil)

(autoload 'evil-avy-mode "evil-avy" "\
Toggle evil-avy-mode.
Interactively with no argument, this command toggles the mode. A
positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode,`toggle' toggles the state.

If called interactively, enable Evil-Avy mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

When evil-avy-mode is active, it replaces some the normal, visual, operator
and motion state keybindings to invoke avy commands.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-avy" '("avy-forward-char-in-line")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-avy-autoloads.el ends here
;;; evil-collection-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-collection" "evil-collection.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from evil-collection.el

(autoload 'evil-collection-translate-key "evil-collection" "\
Translate keys in the keymap(s) corresponding to STATES and KEYMAPS.
STATES should be the name of an evil state, a list of states, or nil. KEYMAPS
should be a symbol corresponding to the keymap to make the translations in or a
list of keymap symbols. Like `evil-define-key', when a keymap does not exist,
the keybindings will be deferred until the keymap is defined, so
`with-eval-after-load' is not necessary. TRANSLATIONS corresponds to a list of
key replacement pairs. For example, specifying \"a\" \"b\" will bind \"a\" to
\"b\"'s definition in the keymap. Specifying nil as a replacement will unbind a
key. If DESTRUCTIVE is nil, a backup of the keymap will be stored on the initial
invocation, and future invocations will always look up keys in the backup
keymap. When no TRANSLATIONS are given, this function will only create the
backup keymap without making any translations. On the other hand, if DESTRUCTIVE
is non-nil, the keymap will be destructively altered without creating a backup.
For example, calling this function multiple times with \"a\" \"b\" \"b\" \"a\"
would continue to swap and unswap the definitions of these keys. This means that
when DESTRUCTIVE is non-nil, all related swaps/cycles should be done in the same
invocation.

\(fn STATES KEYMAPS &rest TRANSLATIONS &key DESTRUCTIVE &allow-other-keys)" nil nil)

(function-put 'evil-collection-translate-key 'lisp-indent-function 'defun)

(autoload 'evil-collection-swap-key "evil-collection" "\
Wrapper around `evil-collection-translate-key' for swapping keys.
STATES, KEYMAPS, and ARGS are passed to `evil-collection-translate-key'. ARGS
should consist of key swaps (e.g. \"a\" \"b\" is equivalent to \"a\" \"b\" \"b\"
\"a\" with `evil-collection-translate-key') and optionally keyword arguments for
`evil-collection-translate-key'.

\(fn STATES KEYMAPS &rest ARGS)" nil t)

(function-put 'evil-collection-swap-key 'lisp-indent-function 'defun)

(autoload 'evil-collection-require "evil-collection" "\
Require the evil-collection-MODE file, but do not activate it.

MODE should be a symbol. This requires the evil-collection-MODE
feature without needing to manipulate `load-path'. NOERROR is
forwarded to `require'.

\(fn MODE &optional NOERROR)" nil nil)

(autoload 'evil-collection-init "evil-collection" "\
Register the Evil bindings for all modes in `evil-collection-mode-list'.

Alternatively, you may register select bindings manually, for
instance:

  (with-eval-after-load 'calendar
    (evil-collection-calendar-setup))

If MODES is specified (as either one mode or a list of modes), use those modes
instead of the modes in `evil-collection-mode-list'.

\(fn &optional MODES)" t nil)

(register-definition-prefixes "evil-collection" '("evil-collection-"))

;;;***

;;;### (autoloads nil nil ("evil-collection-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-collection-autoloads.el ends here
;;; evil-mark-replace-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-mark-replace" "evil-mark-replace.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-mark-replace.el

(autoload 'evilmr-replace "evil-mark-replace" "\
Mark region with MARK-FN and replace in marked area.

\(fn MARK-FN)" nil nil)

(autoload 'evilmr-show-tagged-region "evil-mark-replace" "\
Mark and show tagged region." t nil)

(autoload 'evilmr-tag-selected-region "evil-mark-replace" "\
Tag selected region." t nil)

(autoload 'evilmr-replace-in-buffer "evil-mark-replace" "\
Mark buffer and replace the thing." t nil)

(autoload 'evilmr-replace-in-defun "evil-mark-replace" "\
Mark defun and replace the thing." t nil)

(autoload 'evilmr-replace-in-tagged-region "evil-mark-replace" "\
Mark tagged region and replace the thing." t nil)

(autoload 'evilmr-version "evil-mark-replace" "\
Print current version." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-mark-replace" '("evilmr-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-mark-replace-autoloads.el ends here
;;; evil-matchit-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-matchit" "evil-matchit.el" (0 0 0 0))
;;; Generated autoloads from evil-matchit.el

(autoload 'evilmi-load-plugin-rules "evil-matchit" "\
Load MODES's plugin RULES.

\(fn MODES RULES)" nil nil)

(autoload 'evilmi-init-plugins "evil-matchit" "\
Load plugins." t nil)

(autoload 'evilmi-select-items "evil-matchit" "\
Select NUM items/tags and the region between them.

\(fn &optional NUM)" t nil)

(autoload 'evilmi-delete-items "evil-matchit" "\
Delete NUM items/tags and the region between them.

\(fn &optional NUM)" t nil)

(autoload 'evilmi-jump-to-percentage "evil-matchit" "\
Like Vim %, NUM is the percentage of location.

\(fn NUM)" t nil)
 (autoload 'evilmi-jump-items "evil-matchit" nil t)

(autoload 'evilmi-version "evil-matchit" "\
Print version." t nil)

(autoload 'evil-matchit-mode "evil-matchit" "\
Buffer-local minor mode to emulate matchit.vim.

If called interactively, toggle `Evil-Matchit mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-matchit-mode "evil-matchit" "\
Enable the minor mode in the current buffer." nil nil)

(autoload 'turn-off-evil-matchit-mode "evil-matchit" "\
Disable the minor mode in the current buffer." nil nil)

(put 'global-evil-matchit-mode 'globalized-minor-mode t)

(defvar global-evil-matchit-mode nil "\
Non-nil if Global Evil-Matchit mode is enabled.
See the `global-evil-matchit-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-matchit-mode'.")

(custom-autoload 'global-evil-matchit-mode "evil-matchit" nil)

(autoload 'global-evil-matchit-mode "evil-matchit" "\
Toggle Evil-Matchit mode in all buffers.
With prefix ARG, enable Global Evil-Matchit mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG
is omitted or nil.

Evil-Matchit mode is enabled in all buffers where
`turn-on-evil-matchit-mode' would do it.

See `evil-matchit-mode' for more information on Evil-Matchit mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "evil-matchit" '("evil"))

;;;***

;;;### (autoloads nil "evil-matchit-c" "evil-matchit-c.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from evil-matchit-c.el

(autoload 'evilmi-c-get-tag "evil-matchit-c" "\
Get tag at point." nil nil)

(autoload 'evilmi-c-jump "evil-matchit-c" "\
Use INFO to jump NUM times.

\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-c" '("evilmi-c-"))

;;;***

;;;### (autoloads nil "evil-matchit-cmake" "evil-matchit-cmake.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-cmake.el

(autoload 'evilmi-cmake-get-tag "evil-matchit-cmake" nil nil nil)

(autoload 'evilmi-cmake-jump "evil-matchit-cmake" "\


\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-cmake" '("evilmi-cmake-"))

;;;***

;;;### (autoloads nil "evil-matchit-diff" "evil-matchit-diff.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-diff.el

(autoload 'evilmi-diff-get-tag "evil-matchit-diff" "\
Get tag at point." nil nil)

(autoload 'evilmi-diff-jump "evil-matchit-diff" "\
Jump to the matching tag using INFO and NUM.

\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-diff" '("evilmi-diff-"))

;;;***

;;;### (autoloads nil "evil-matchit-elixir" "evil-matchit-elixir.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-elixir.el

(autoload 'evilmi-elixir-get-tag "evil-matchit-elixir" nil nil nil)

(autoload 'evilmi-elixir-jump "evil-matchit-elixir" "\


\(fn RLT NUM)" nil nil)

(register-definition-prefixes "evil-matchit-elixir" '("evilmi-elixir-"))

;;;***

;;;### (autoloads nil "evil-matchit-fortran" "evil-matchit-fortran.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-fortran.el

(autoload 'evilmi-fortran-get-tag "evil-matchit-fortran" nil nil nil)

(autoload 'evilmi-fortran-jump "evil-matchit-fortran" "\


\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-fortran" '("evilmi-fortran-"))

;;;***

;;;### (autoloads nil "evil-matchit-html" "evil-matchit-html.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-html.el

(autoload 'evilmi-html-get-tag "evil-matchit-html" "\
Get current tag." nil nil)

(autoload 'evilmi-html-jump "evil-matchit-html" "\
Use INFO from current tag to jump NUM times.

\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-html" '("evilmi-html--"))

;;;***

;;;### (autoloads nil "evil-matchit-javascript" "evil-matchit-javascript.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-javascript.el

(autoload 'evilmi-javascript-get-tag "evil-matchit-javascript" "\
Get tag at point." nil nil)

(autoload 'evilmi-javascript-jump "evil-matchit-javascript" "\
Jump to the matching tag using INFO and NUM.

\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-javascript" '("evilmi-"))

;;;***

;;;### (autoloads nil "evil-matchit-latex" "evil-matchit-latex.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-latex.el

(autoload 'evilmi-latex-get-tag "evil-matchit-latex" nil nil nil)

(autoload 'evilmi-latex-jump "evil-matchit-latex" "\


\(fn RLT NUM)" nil nil)

(register-definition-prefixes "evil-matchit-latex" '("evilmi-latex-"))

;;;***

;;;### (autoloads nil "evil-matchit-markdown" "evil-matchit-markdown.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-markdown.el

(autoload 'evilmi-markdown-get-tag "evil-matchit-markdown" "\
Get current tag.  Return (list start-position tag)." nil nil)

(autoload 'evilmi-markdown-jump "evil-matchit-markdown" "\
Jump to the next tag using INFO and NUM.

\(fn INFO NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-ocaml" "evil-matchit-ocaml.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-ocaml.el

(autoload 'evilmi-ocaml-get-tag "evil-matchit-ocaml" "\
Return information of current tag: (list position-of-word word)." nil nil)

(autoload 'evilmi-ocaml-jump "evil-matchit-ocaml" "\


\(fn RLT NUM)" nil nil)

(register-definition-prefixes "evil-matchit-ocaml" '("evilmi-ocaml-"))

;;;***

;;;### (autoloads nil "evil-matchit-octave" "evil-matchit-octave.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-octave.el

(autoload 'evilmi-octave-get-tag "evil-matchit-octave" "\
Get current tag info." nil nil)

(autoload 'evilmi-octave-jump "evil-matchit-octave" "\
Use INFO returned by `evilmi-octave-get-tag' and NUM to jump to matched tag.

\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-octave" '("evilmi-octave-"))

;;;***

;;;### (autoloads nil "evil-matchit-org" "evil-matchit-org.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from evil-matchit-org.el

(autoload 'evilmi-org-get-tag "evil-matchit-org" "\
Get current tag in org file." nil nil)

(autoload 'evilmi-org-jump "evil-matchit-org" "\
Jump to the matching tag using INFO and NUM.

\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-org" '("evilmi-"))

;;;***

;;;### (autoloads nil "evil-matchit-python" "evil-matchit-python.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-python.el

(autoload 'evilmi-python-get-tag "evil-matchit-python" "\
Return '(start-position tag-type keyword)." nil nil)

(autoload 'evilmi-python-jump "evil-matchit-python" "\
Use INFO returned by `evilmi-python-get-tag' and NUM to jump to matched tag.

\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-python" '("evilmi--python-"))

;;;***

;;;### (autoloads nil "evil-matchit-ruby" "evil-matchit-ruby.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-ruby.el

(autoload 'evilmi-ruby-get-tag "evil-matchit-ruby" "\
Get tag at point." nil nil)

(autoload 'evilmi-ruby-jump "evil-matchit-ruby" "\
Use INFO to jump NUM times.

\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-ruby" '("evilmi-ruby-"))

;;;***

;;;### (autoloads nil "evil-matchit-script" "evil-matchit-script.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-script.el

(autoload 'evilmi-script-get-tag "evil-matchit-script" "\
Get tag at point." nil nil)

(autoload 'evilmi-script-jump "evil-matchit-script" "\
Use INFO returned by `evilmi-script-get-tag' and NUM to jump to matched tag.

\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-script" '("evilmi-script-"))

;;;***

;;;### (autoloads nil "evil-matchit-sdk" "evil-matchit-sdk.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from evil-matchit-sdk.el

(autoload 'evilmi-sdk-curline "evil-matchit-sdk" "\
Get current line text." nil nil)

(autoload 'evilmi-sdk-member "evil-matchit-sdk" "\
Check if KEYWORD exist in KEYWORD-LIST.

\(fn KEYWORD KEYWORD-LIST)" nil nil)

(autoload 'evilmi-sdk-get-tag-info "evil-matchit-sdk" "\
Return (row column is-function-exit-point keyword).
The row and column mark the position in `evilmi-mylang-match-tags'
is-function-exit-point could be unknown status

\(fn KEYWORD MATCH-TAGS)" nil nil)

(autoload 'evilmi-sdk-get-tag "evil-matchit-sdk" "\
Return '(start-point ((row column is-function-exit-point keyword)).

\(fn MATCH-TAGS HOWTOS)" nil nil)

(autoload 'evilmi-sdk-jump "evil-matchit-sdk" "\
Use RLT, NUM, MATCH-TAGS and HOWTOS to jump.
Return nil if no matching tag found.  Please note (point) is changed
after calling this function.

\(fn RLT NUM MATCH-TAGS HOWTOS)" nil nil)

(autoload 'evilmi-among-fonts-p "evil-matchit-sdk" "\
If current font at POS is among FONTS.

\(fn POS FONTS)" nil nil)

(register-definition-prefixes "evil-matchit-sdk" '("evilmi-"))

;;;***

;;;### (autoloads nil "evil-matchit-sh" "evil-matchit-sh.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from evil-matchit-sh.el

(autoload 'evilmi-sh-get-tag "evil-matchit-sh" nil nil nil)

(autoload 'evilmi-sh-jump "evil-matchit-sh" "\


\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-sh" '("evilmi-sh-"))

;;;***

;;;### (autoloads nil "evil-matchit-simple" "evil-matchit-simple.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-simple.el

(autoload 'evilmi-simple-get-tag "evil-matchit-simple" "\
Get current tag in simple language." nil nil)

(autoload 'evilmi-simple-jump "evil-matchit-simple" "\
Use INFO of current tag to jump to matching tag.  NUM is ignored.

\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-simple" '("evilmi-"))

;;;***

;;;### (autoloads nil "evil-matchit-sql" "evil-matchit-sql.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from evil-matchit-sql.el

(autoload 'evilmi-sql-get-tag "evil-matchit-sql" "\
Get tag at point." nil nil)

(autoload 'evilmi-sql-jump "evil-matchit-sql" "\
Use INFO returned by `evilmi-sql-get-tag' and NUM to jump to matched tag.

\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-sql" '("evilmi-sql-"))

;;;***

;;;### (autoloads nil "evil-matchit-template" "evil-matchit-template.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-template.el

(autoload 'evilmi-template-get-tag "evil-matchit-template" "\
Get tag at point." nil nil)

(autoload 'evilmi-template-jump "evil-matchit-template" "\
Jump to the matching tag using INFO and NUM.

\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-template" '("evilmi-template-"))

;;;***

;;;### (autoloads nil "evil-matchit-terminal" "evil-matchit-terminal.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-terminal.el

(autoload 'evilmi-prompt-line-p "evil-matchit-terminal" "\
If line at POSITION has prompt at the beginning.

\(fn &optional POSITION)" nil nil)

(autoload 'evilmi-terminal-get-tag "evil-matchit-terminal" "\
Get tag at point." nil nil)

(autoload 'evilmi-terminal-jump "evil-matchit-terminal" "\
Use INFO to jump NUM times.

\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-terminal" '("evilmi-terminal-p"))

;;;***

;;;### (autoloads nil "evil-matchit-verilog" "evil-matchit-verilog.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-matchit-verilog.el

(autoload 'evilmi-verilog-get-tag "evil-matchit-verilog" "\
Get tag at point." nil nil)

(autoload 'evilmi-verilog-jump "evil-matchit-verilog" "\
Use INFO returned by `evilmi-verlog-get-tag' and NUM to jump to matched tag.

\(fn INFO NUM)" nil nil)

(register-definition-prefixes "evil-matchit-verilog" '("evilmi-verilog-"))

;;;***

;;;### (autoloads nil nil ("evil-matchit-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-matchit-autoloads.el ends here
;;; evil-nerd-commenter-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-nerd-commenter" "evil-nerd-commenter.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-nerd-commenter.el

(autoload 'evilnc-comment-or-uncomment-region-internal "evil-nerd-commenter" "\
Comment or uncomment region from BEG to END.

\(fn BEG END)" nil nil)

(autoload 'evilnc-comment-or-uncomment-region "evil-nerd-commenter" "\
Comment or uncomment region from BEG to END.

\(fn BEG END)" nil nil)

(autoload 'evilnc-comment-or-uncomment-paragraphs "evil-nerd-commenter" "\
Comment or uncomment NUM paragraph(s).
A paragraph is a continuation non-empty lines.
Paragraphs are separated by empty lines.

\(fn &optional NUM)" t nil)

(autoload 'evilnc-comment-or-uncomment-to-the-line "evil-nerd-commenter" "\
Comment or uncomment from current line to LINENUM line.

\(fn &optional LINENUM)" t nil)

(autoload 'evilnc-quick-comment-or-uncomment-to-the-line "evil-nerd-commenter" "\
Comment/uncomment to line number by LAST-DIGITS.
For example, you can use either \\<M-53>\\[evilnc-quick-comment-or-uncomment-to-the-line] or \\<M-3>\\[evilnc-quick-comment-or-uncomment-to-the-line] to comment to the line 6453

\(fn &optional LAST-DIGITS)" t nil)

(autoload 'evilnc-toggle-invert-comment-line-by-line "evil-nerd-commenter" "\
Please note this command may NOT work on complex evil text objects." t nil)

(autoload 'evilnc-toggle-comment-empty-lines "evil-nerd-commenter" "\
Toggle the flag which decide if empty line will be commented." t nil)

(autoload 'evilnc-comment-or-uncomment-lines "evil-nerd-commenter" "\
Comment or uncomment NUM lines.  NUM could be negative.

Case 1: If no region selected, comment/uncomment on current line.
If NUM>1, comment/uncomment extra N-1 lines from next line.

Case 2: Selected region is expanded to make it contain whole lines.
Then we comment/uncomment the expanded region.  NUM is ignored.

Case 3: If a region inside of ONE line is selected,
we comment/uncomment that region.
CORRECT comment syntax will be used for C++/Java/Javascript.

\(fn &optional NUM)" t nil)

(autoload 'evilnc-copy-and-comment-lines "evil-nerd-commenter" "\
Copy&paste NUM lines and comment out original lines.
NUM could be negative.

Case 1: If no region selected, operate on current line.
if NUM>1, comment/uncomment extra N-1 lines from next line

Case 2: Selected region is expanded to make it contain whole lines.
Then we operate the expanded region.  NUM is ignored.

\(fn &optional NUM)" t nil)

(autoload 'evilnc-comment-and-kill-ring-save "evil-nerd-commenter" "\
Comment lines save origin lines into `kill-ring'.
NUM could be negative.

Case 1: If no region selected, operate on current line.
;; if NUM>1, comment/uncomment extra N-1 lines from next line

Case 2: Selected region is expanded to make it contain whole lines.
Then we operate the expanded region.  NUM is ignored.

\(fn &optional NUM)" t nil)

(autoload 'evilnc-copy-to-line "evil-nerd-commenter" "\
Copy from current line to LINENUM line.  For non-evil user only.

\(fn &optional LINENUM)" t nil)

(autoload 'evilnc-kill-to-line "evil-nerd-commenter" "\
Kill from the current line to the LINENUM line.  For non-evil user only.

\(fn &optional LINENUM)" t nil)

(autoload 'evilnc-version "evil-nerd-commenter" "\
The version number." t nil)

(autoload 'evilnc-default-hotkeys "evil-nerd-commenter" "\
Setup the key bindings of evil-nerd-comment.
If NO-EVIL-KEYBINDINGS is t, we don't define keybindings in EVIL,
if NO-EMACS-KEYBINDINGS is t, we don't define keybindings in EMACS mode.

\(fn &optional NO-EVIL-KEYBINDINGS NO-EMACS-KEYBINDINGS)" t nil)

(autoload 'evilnc-imenu-create-index-function "evil-nerd-commenter" "\
Imenu function find comments." nil nil)

(autoload 'evilnc-comment-or-uncomment-html-tag "evil-nerd-commenter" "\
Comment or uncomment html tag(s).
If no region is selected, current tag under focus is automatically selected.
In this case, only one tag is selected.
If users manually select region, the region could cross multiple sibling tags
and automatically expands to include complete tags.
Users can press \"v\" key in evil mode to select multiple tags.
This command is not dependent on any 3rd party package." t nil)

(autoload 'evilnc-comment-or-uncomment-html-paragraphs "evil-nerd-commenter" "\
Comment or uncomment NUM paragraphs contain html tag.
A paragraph is a continuation non-empty lines.
Paragraphs are separated by empty lines.

\(fn &optional NUM)" t nil)

(register-definition-prefixes "evil-nerd-commenter" '("evilnc-"))

;;;***

;;;### (autoloads nil "evil-nerd-commenter-operator" "evil-nerd-commenter-operator.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-nerd-commenter-operator.el

(register-definition-prefixes "evil-nerd-commenter-operator" '("evilnc-"))

;;;***

;;;### (autoloads nil "evil-nerd-commenter-sdk" "evil-nerd-commenter-sdk.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-nerd-commenter-sdk.el

(register-definition-prefixes "evil-nerd-commenter-sdk" '("evilnc-"))

;;;***

;;;### (autoloads nil nil ("evil-nerd-commenter-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-nerd-commenter-autoloads.el ends here
;;; evil-paredit-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-paredit" "evil-paredit.el" (0 0 0 0))
;;; Generated autoloads from evil-paredit.el

(autoload 'evil-paredit-mode "evil-paredit" "\
Minor mode for setting up Evil with paredit in a single buffer

If called interactively, toggle `Evil-Paredit mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "evil-paredit" '("-evil-paredit-check-region" "evil-paredit-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-paredit-autoloads.el ends here
;;; evil-surround-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-surround" "evil-surround.el" (0 0 0 0))
;;; Generated autoloads from evil-surround.el

(autoload 'evil-surround-delete "evil-surround" "\
Delete the surrounding delimiters represented by CHAR.
Alternatively, the text to delete can be represented with
the overlays OUTER and INNER, where OUTER includes the delimiters
and INNER excludes them. The intersection (i.e., difference)
between these overlays is what is deleted.

\(fn CHAR &optional OUTER INNER)" t nil)

(autoload 'evil-surround-change "evil-surround" "\
Change the surrounding delimiters represented by CHAR.
Alternatively, the text to delete can be represented with the
overlays OUTER and INNER, which are passed to `evil-surround-delete'.

\(fn CHAR &optional OUTER INNER)" t nil)

(autoload 'evil-surround-mode "evil-surround" "\
Buffer-local minor mode to emulate surround.vim.

If called interactively, enable Evil-Surround mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-surround-mode "evil-surround" "\
Enable evil-surround-mode in the current buffer." nil nil)

(autoload 'turn-off-evil-surround-mode "evil-surround" "\
Disable evil-surround-mode in the current buffer." nil nil)

(put 'global-evil-surround-mode 'globalized-minor-mode t)

(defvar global-evil-surround-mode nil "\
Non-nil if Global Evil-Surround mode is enabled.
See the `global-evil-surround-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-surround-mode'.")

(custom-autoload 'global-evil-surround-mode "evil-surround" nil)

(autoload 'global-evil-surround-mode "evil-surround" "\
Toggle Evil-Surround mode in all buffers.
With prefix ARG, enable Global Evil-Surround mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Surround mode is enabled in all buffers where
`turn-on-evil-surround-mode' would do it.
See `evil-surround-mode' for more information on Evil-Surround mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-surround" '("evil-surround-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-surround-autoloads.el ends here
;;; exec-path-from-shell-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "exec-path-from-shell" "exec-path-from-shell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from exec-path-from-shell.el

(autoload 'exec-path-from-shell-copy-envs "exec-path-from-shell" "\
Set the environment variables with NAMES from the user's shell.

As a special case, if the variable is $PATH, then the variables
`exec-path' and `eshell-path-env' are also set appropriately.
The result is an alist, as described by
`exec-path-from-shell-getenvs'.

\(fn NAMES)" nil nil)

(autoload 'exec-path-from-shell-copy-env "exec-path-from-shell" "\
Set the environment variable $NAME from the user's shell.

As a special case, if the variable is $PATH, then the variables
`exec-path' and `eshell-path-env' are also set appropriately.
Return the value of the environment variable.

\(fn NAME)" t nil)

(autoload 'exec-path-from-shell-initialize "exec-path-from-shell" "\
Initialize environment from the user's shell.

The values of all the environment variables named in
`exec-path-from-shell-variables' are set from the corresponding
values used in the user's shell." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exec-path-from-shell" '("exec-path-from-shell-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; exec-path-from-shell-autoloads.el ends here
;;; f-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "f" "f.el" (0 0 0 0))
;;; Generated autoloads from f.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "f" '("f-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; f-autoloads.el ends here
;;; find-file-in-project-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "find-file-in-project" "find-file-in-project.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from find-file-in-project.el

(autoload 'ffip-git-diff-current-file "find-file-in-project" "\
Run 'git diff version:current-file current-file'." nil nil)

(autoload 'ffip-copy-without-change "find-file-in-project" "\
Copy P without change.

\(fn P)" nil nil)

(autoload 'ffip-copy-reactjs-import "find-file-in-project" "\
Create ReactJS link from P and copy the result.

\(fn P)" nil nil)

(autoload 'ffip-copy-org-file-link "find-file-in-project" "\
Create org link from P and copy the result.

\(fn P)" nil nil)

(defvar ffip-find-relative-path-callback 'ffip-copy-without-change "\
The callback after calling `find-relative-path'.")

(custom-autoload 'ffip-find-relative-path-callback "find-file-in-project" t)

(autoload 'ffip-project-root "find-file-in-project" "\
Return project root or `default-directory'." nil nil)

(autoload 'ffip-get-project-root-directory "find-file-in-project" "\
Get the full path of project root directory." nil nil)

(autoload 'ffip-filename-identity "find-file-in-project" "\
Return identical KEYWORD.

\(fn KEYWORD)" nil nil)

(autoload 'ffip-filename-camelcase-to-dashes "find-file-in-project" "\
Convert KEYWORD from camel cased to dash separated.
If CHECK-ONLY is true, only do the check.

\(fn KEYWORD &optional CHECK-ONLY)" nil nil)

(autoload 'ffip-filename-dashes-to-camelcase "find-file-in-project" "\
Convert KEYWORD from dash separated to camel cased.
If CHECK-ONLY is true, only do the check.

\(fn KEYWORD &optional CHECK-ONLY)" nil nil)

(autoload 'ffip-completing-read "find-file-in-project" "\
Read a string in minibuffer, with completion.

PROMPT is a string with same format parameters in `completing-read'.
COLLECTION is a list of strings.

ACTION is a lambda function to call after selecting a result.

This function returns the selected candidate or nil.

\(fn PROMPT COLLECTION &optional ACTION)" nil nil)

(autoload 'ffip-project-search "find-file-in-project" "\
Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique.

If KEYWORD is string, it's the file name or file path to find file.
If KEYWORD is list, it's the list of file names.
IF FIND-DIRECTORY-P is t, we are searching directories, else files.

\(fn KEYWORD &optional FIND-DIRECTORY-P)" nil nil)

(autoload 'ffip-find-files "find-file-in-project" "\
Use KEYWORD to find files.
If OPEN-ANOTHER-WINDOW is t, the results are displayed in a new window.
If FIND-DIRECTORY-P is t, only search directories.  FN is callback.
This function is the API to find files.

\(fn KEYWORD OPEN-ANOTHER-WINDOW &optional FIND-DIRECTORY-P FN)" nil nil)

(autoload 'ffip-create-project-file "find-file-in-project" "\
Create or Append .dir-locals.el to set up per directory.
You can move .dir-locals.el to root directory.
See (info \"(Emacs) Directory Variables\") for details." t nil)

(autoload 'ffip-current-full-filename-match-pattern-p "find-file-in-project" "\
Is current full file name (including directory) match the REGEX?

\(fn REGEX)" nil nil)

(autoload 'find-file-in-project "find-file-in-project" "\
More powerful and efficient `find-file-in-project-by-selected' is recommended.

Prompt with a completing list of all files in the project to find one.
If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window.
The project's scope is defined as the first directory containing
a `ffip-project-file' whose value is \".git\" by default.
You can override this by setting the variable `ffip-project-root'.

\(fn &optional OPEN-ANOTHER-WINDOW)" t nil)

(autoload 'find-file-in-project-at-point "find-file-in-project" "\
Find file whose name is guessed around point.
If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window.

\(fn &optional OPEN-ANOTHER-WINDOW)" t nil)

(autoload 'find-file-in-current-directory "find-file-in-project" "\
Search file in current directory or LEVEL up parent directory.

\(fn &optional LEVEL)" t nil)

(autoload 'find-file-in-project-by-selected "find-file-in-project" "\
Same as `find-file-in-project' but more powerful and faster.
It use string from selected region to search files in the project.
If no region is selected, you could provide a keyword.

Keyword could be ANY part of the file's full path and support wildcard.
For example, to find /home/john/proj1/test.js, below keywords are valid:
- test.js
- roj1/tes
- john*test

If keyword contains line number like \"hello.txt:32\" or \"hello.txt:32:\",
we will move to that line in opened file.

If keyword is empty, it behaves same as `find-file-in-project'.

If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window.

\(fn &optional OPEN-ANOTHER-WINDOW)" t nil)

(autoload 'ffip-insert-file "find-file-in-project" "\
Insert contents of file in current buffer.
The file name is selected interactively from candidates in project." t nil)

(autoload 'find-file-with-similar-name "find-file-in-project" "\
Use base name of current file as keyword which could be further stripped.
by `ffip-strip-file-name-regex'.
If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window.

\(fn &optional OPEN-ANOTHER-WINDOW)" t nil)

(autoload 'find-file-in-current-directory-by-selected "find-file-in-project" "\
Like `find-file-in-project-by-selected' but search current directory.
If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window.

\(fn &optional OPEN-ANOTHER-WINDOW)" t nil)

(autoload 'ffip-find-relative-path "find-file-in-project" "\
Find file/directory and copy its relative path into `kill-ring'.
If FIND-DIRECTORY-P is t, copy the directory path.

Set `ffip-find-relative-path-callback' to format the result,
  (setq ffip-find-relative-path-callback 'ffip-copy-reactjs-import)
  (setq ffip-find-relative-path-callback 'ffip-copy-org-file-link)

\(fn &optional FIND-DIRECTORY-P)" t nil)

(autoload 'find-directory-in-project-by-selected "find-file-in-project" "\
Similar to `find-file-in-project-by-selected'.
Use string from selected region to find directory in the project.
If no region is selected, you need provide keyword.

Keyword could be directory's base-name only or parent-directory+base-name
For example, to find /home/john/proj1/test, below keywords are valid:
- test
- roj1/test
- john*test

If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window.

\(fn &optional OPEN-ANOTHER-WINDOW)" t nil)

(autoload 'ffip-lisp-find-file-in-project "find-file-in-project" "\
If DIRECTORY-P is nil, find file in project, or else find directory.
This command works in any environment (Windows, etc) out of box.

\(fn &optional DIRECTORY-P)" t nil)

(defalias 'ffip 'find-file-in-project)

(autoload 'ffip-diff-quit "find-file-in-project" "\
Quit." t nil)

(autoload 'ffip-diff-find-file "find-file-in-project" "\
File file(s) in current hunk.
If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window.

\(fn &optional OPEN-ANOTHER-WINDOW)" t nil)

(autoload 'ffip-show-diff-internal "find-file-in-project" "\
Show the diff output by executing selected `ffip-diff-backends'.
NUM is the index selected backend from `ffip-diff-backends'.
NUM is zero based whose default value is zero.

\(fn &optional NUM)" t nil)

(autoload 'ffip-show-diff-by-description "find-file-in-project" "\
Show the diff output by executing selected `ffip-diff-backends'.
NUM is the backend index of `ffip-diff-backends'.
If NUM is not nil, the corresponding backend is executed directly.

\(fn &optional NUM)" t nil)

(autoload 'ffip-diff-apply-hunk "find-file-in-project" "\
Apply current hunk in `diff-mode'.  Try to locate the file to patch.
Similar to `diff-apply-hunk' but smarter.
Please read documentation of `diff-apply-hunk' to get more details.
If REVERSE is t, applied patch is reverted.

\(fn &optional REVERSE)" t nil)

(autoload 'ffip-fix-file-path-at-point "find-file-in-project" "\
Fix file path at point.
If ABSOLUTE-PATH-P is t, old path is replaced by correct absolute path.
Or else it's replaced by relative path.

\(fn &optional ABSOLUTE-PATH-P)" t nil)

(register-definition-prefixes "find-file-in-project" '("ffip-" "find-relative-path"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; find-file-in-project-autoloads.el ends here
;;; flycheck-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck" "flycheck.el" (0 0 0 0))
;;; Generated autoloads from flycheck.el

(autoload 'flycheck-manual "flycheck" "\
Open the Flycheck manual." t nil)

(autoload 'flycheck-mode "flycheck" "\
Flycheck is a minor mode for on-the-fly syntax checking.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable syntax checker from `flycheck-checkers'.
Use `flycheck-select-checker' to select a checker for the current
buffer manually.

If you run into issues, use `\\[flycheck-verify-setup]' to get help.

Flycheck supports many languages out of the box, and many
additional ones are available on MELPA.  Adding new ones is very
easy.  Complete documentation is available online at URL
`https://www.flycheck.org/en/latest/'.  Please report issues and
request features at URL `https://github.com/flycheck/flycheck'.

Flycheck displays its status in the mode line.  In the default
configuration, it looks like this:

`FlyC'     This buffer has not been checked yet.
`FlyC-'    Flycheck doesn't have a checker for this buffer.
`FlyC*'    Flycheck is running.  Expect results soon!
`FlyC:3|2' This buffer contains three warnings and two errors.
           Use `\\[flycheck-list-errors]' to see the list.

You may also see the following icons:
`FlyC!'    The checker crashed.
`FlyC.'    The last syntax check was manually interrupted.
`FlyC?'    The checker did something unexpected, like exiting with 1
           but returning no errors.

The following keybindings are available in `flycheck-mode':

\\{flycheck-mode-map}
\(you can change the prefix by customizing
`flycheck-keymap-prefix')

If called interactively, enable Flycheck mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is ‘toggle’; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-flycheck-mode 'globalized-minor-mode t)

(defvar global-flycheck-mode nil "\
Non-nil if Global Flycheck mode is enabled.
See the `global-flycheck-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-flycheck-mode'.")

(custom-autoload 'global-flycheck-mode "flycheck" nil)

(autoload 'global-flycheck-mode "flycheck" "\
Toggle Flycheck mode in all buffers.
With prefix ARG, enable Global Flycheck mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG is
omitted or nil.

Flycheck mode is enabled in all buffers where
`flycheck-mode-on-safe' would do it.

See `flycheck-mode' for more information on Flycheck mode.

\(fn &optional ARG)" t nil)

(autoload 'flycheck-define-error-level "flycheck" "\
Define a new error LEVEL with PROPERTIES.

The following PROPERTIES constitute an error level:

`:severity SEVERITY'
     A number denoting the severity of this level.  The higher
     the number, the more severe is this level compared to other
     levels.  Defaults to 0; info is -10, warning is 10, and
     error is 100.

     The severity is used by `flycheck-error-level-<' to
     determine the ordering of errors according to their levels.

`:compilation-level LEVEL'

     A number indicating the broad class of messages that errors
     at this level belong to: one of 0 (info), 1 (warning), or
     2 or nil (error).  Defaults to nil.

     This is used by `flycheck-checker-pattern-to-error-regexp'
     to map error levels into `compilation-mode''s hierarchy and
     to get proper highlighting of errors in `compilation-mode'.

`:overlay-category CATEGORY'
     A symbol denoting the overlay category to use for error
     highlight overlays for this level.  See Info
     node `(elisp)Overlay Properties' for more information about
     overlay categories.

     A category for an error level overlay should at least define
     the `face' property, for error highlighting.  Another useful
     property for error level categories is `priority', to
     influence the stacking of multiple error level overlays.

`:fringe-bitmap BITMAPS'
     A fringe bitmap symbol denoting the bitmap to use for fringe
     indicators for this level, or a cons of two bitmaps (one for
     narrow fringes and one for wide fringes).  See Info node
     `(elisp)Fringe Bitmaps' for more information about fringe
     bitmaps, including a list of built-in fringe bitmaps.

`:fringe-face FACE'
     A face symbol denoting the face to use for fringe indicators
     for this level.

`:margin-spec SPEC'
     A display specification indicating what to display in the
     margin when `flycheck-indication-mode' is `left-margin' or
     `right-margin'.  See Info node `(elisp)Displaying in the
     Margins'.  If omitted, Flycheck generates an image spec from
     the fringe bitmap.

`:error-list-face FACE'
     A face symbol denoting the face to use for messages of this
     level in the error list.  See `flycheck-list-errors'.

\(fn LEVEL &rest PROPERTIES)" nil nil)

(function-put 'flycheck-define-error-level 'lisp-indent-function '1)

(autoload 'flycheck-define-command-checker "flycheck" "\
Define SYMBOL as syntax checker to run a command.

Define SYMBOL as generic syntax checker via
`flycheck-define-generic-checker', which uses an external command
to check the buffer.  SYMBOL and DOCSTRING are the same as for
`flycheck-define-generic-checker'.

In addition to the properties understood by
`flycheck-define-generic-checker', the following PROPERTIES
constitute a command syntax checker.  Unless otherwise noted, all
properties are mandatory.  Note that the default `:error-filter'
of command checkers is `flycheck-sanitize-errors'.

`:command COMMAND'
     The command to run for syntax checking.

     COMMAND is a list of the form `(EXECUTABLE [ARG ...])'.

     EXECUTABLE is a string with the executable of this syntax
     checker.  It can be overridden with the variable
     `flycheck-SYMBOL-executable'.  Note that this variable is
     NOT implicitly defined by this function.  Use
     `flycheck-def-executable-var' to define this variable.

     Each ARG is an argument to the executable, either as string,
     or as special symbol or form for
     `flycheck-substitute-argument', which see.

`:error-patterns PATTERNS'
     A list of patterns to parse the output of the `:command'.

     Each ITEM in PATTERNS is a list `(LEVEL SEXP ...)', where
     LEVEL is a Flycheck error level (see
     `flycheck-define-error-level'), followed by one or more RX
     `SEXP's which parse an error of that level and extract line,
     column, file name and the message.

     See `rx' for general information about RX, and
     `flycheck-rx-to-string' for some special RX forms provided
     by Flycheck.

     All patterns are applied in the order of declaration to the
     whole output of the syntax checker.  Output already matched
     by a pattern will not be matched by subsequent patterns.  In
     other words, the first pattern wins.

     This property is optional.  If omitted, however, an
     `:error-parser' is mandatory.

`:error-parser FUNCTION'
     A function to parse errors with.

     The function shall accept three arguments OUTPUT CHECKER
     BUFFER.  OUTPUT is the syntax checker output as string,
     CHECKER the syntax checker that was used, and BUFFER a
     buffer object representing the checked buffer.  The function
     must return a list of `flycheck-error' objects parsed from
     OUTPUT.

     This property is optional.  If omitted, it defaults to
     `flycheck-parse-with-patterns'.  In this case,
     `:error-patterns' is mandatory.

`:standard-input t'
     Whether to send the buffer contents on standard input.

     If this property is given and has a non-nil value, send the
     contents of the buffer on standard input.

     Defaults to nil.

Note that you may not give `:start', `:interrupt', and
`:print-doc' for a command checker.  You can give a custom
`:verify' function, though, whose results will be appended to the
default `:verify' function of command checkers.

\(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil nil)

(function-put 'flycheck-define-command-checker 'lisp-indent-function '1)

(function-put 'flycheck-define-command-checker 'doc-string-elt '2)

(autoload 'flycheck-def-config-file-var "flycheck" "\
Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable using `defcustom', to
provide configuration files for the given syntax CHECKER.
CUSTOM-ARGS are forwarded to `defcustom'.

FILE-NAME is the initial value of the new variable.  If omitted,
the default value is nil.  It can be either a string or a list of
strings.

Use this together with the `config-file' form in the `:command'
argument to `flycheck-define-checker'.

\(fn SYMBOL CHECKER &optional FILE-NAME &rest CUSTOM-ARGS)" nil t)

(function-put 'flycheck-def-config-file-var 'lisp-indent-function '3)

(autoload 'flycheck-def-option-var "flycheck" "\
Define SYMBOL as option variable with INIT-VALUE for CHECKER.

SYMBOL is declared as customizable variable using `defcustom', to
provide an option for the given syntax CHECKERS (a checker or a
list of checkers).  INIT-VALUE is the initial value of the
variable, and DOCSTRING is its docstring.  CUSTOM-ARGS are
forwarded to `defcustom'.

Use this together with the `option', `option-list' and
`option-flag' forms in the `:command' argument to
`flycheck-define-checker'.

\(fn SYMBOL INIT-VALUE CHECKERS DOCSTRING &rest CUSTOM-ARGS)" nil t)

(function-put 'flycheck-def-option-var 'lisp-indent-function '3)

(function-put 'flycheck-def-option-var 'doc-string-elt '4)

(autoload 'flycheck-define-checker "flycheck" "\
Define SYMBOL as command syntax checker with DOCSTRING and PROPERTIES.

Like `flycheck-define-command-checker', but PROPERTIES must not
be quoted.  Also, implicitly define the executable variable for
SYMBOL with `flycheck-def-executable-var'.

\(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil t)

(function-put 'flycheck-define-checker 'lisp-indent-function '1)

(function-put 'flycheck-define-checker 'doc-string-elt '2)

(register-definition-prefixes "flycheck" '("flycheck-" "help-flycheck-checker-d" "list-flycheck-errors"))

;;;***

;;;### (autoloads nil "flycheck-buttercup" "flycheck-buttercup.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flycheck-buttercup.el

(register-definition-prefixes "flycheck-buttercup" '("flycheck-buttercup-format-error-list"))

;;;***

;;;### (autoloads nil "flycheck-ert" "flycheck-ert.el" (0 0 0 0))
;;; Generated autoloads from flycheck-ert.el

(register-definition-prefixes "flycheck-ert" '("flycheck-er"))

;;;***

;;;### (autoloads nil nil ("flycheck-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-autoloads.el ends here
;;; flymake-racket-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flymake-racket" "flymake-racket.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from flymake-racket.el

(autoload 'flymake-racket-setup "flymake-racket" "\
Set up Flymake for Racket." t nil)

(autoload 'flymake-racket-add-hook "flymake-racket" "\
Add `flymake-racket-lint' to `flymake-diagnostic-functions'." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flymake-racket" '("flymake-racket-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flymake-racket-autoloads.el ends here
;;; frame-local-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "frame-local" "frame-local.el" (0 0 0 0))
;;; Generated autoloads from frame-local.el

(register-definition-prefixes "frame-local" '("frame-local-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; frame-local-autoloads.el ends here
;;; geiser-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "geiser" "geiser.el" (0 0 0 0))
;;; Generated autoloads from geiser.el

(defconst geiser-elisp-dir (file-name-directory load-file-name) "\
Directory containing Geiser's Elisp files.")

(autoload 'geiser-version "geiser-version" "\
Echo Geiser's version." t)

(autoload 'geiser-unload "geiser-reload" "\
Unload all Geiser code." t)

(autoload 'geiser-reload "geiser-reload" "\
Reload Geiser code." t)

(autoload 'geiser "geiser-repl" "\
Start a Geiser REPL, or switch to a running one." t)

(autoload 'run-geiser "geiser-repl" "\
Start a Geiser REPL." t)

(autoload 'geiser-connect "geiser-repl" "\
Start a Geiser REPL connected to a remote server." t)

(autoload 'geiser-connect-local "geiser-repl" "\
Start a Geiser REPL connected to a remote server over a Unix-domain socket." t)

(autoload 'switch-to-geiser "geiser-repl" "\
Switch to a running one Geiser REPL." t)

(autoload 'geiser-mode "geiser-mode" "\
Minor mode adding Geiser REPL interaction to Scheme buffers." t)

(autoload 'turn-on-geiser-mode "geiser-mode" "\
Enable Geiser's mode (useful in Scheme buffers)." t)

(autoload 'turn-off-geiser-mode "geiser-mode" "\
Disable Geiser's mode (useful in Scheme buffers)." t)

(autoload 'geiser-mode--maybe-activate "geiser-mode")

(autoload 'geiser-activate-implementation "geiser-impl")

(mapc (lambda (group) (custom-add-load group (symbol-name group)) (custom-add-load 'geiser (symbol-name group))) '(geiser geiser-repl geiser-autodoc geiser-doc geiser-debug geiser-faces geiser-mode geiser-image geiser-implementation geiser-xref))

(add-hook 'scheme-mode-hook 'geiser-mode--maybe-activate)

;;;***

;;;### (autoloads nil "geiser-autodoc" "geiser-autodoc.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from geiser-autodoc.el

(register-definition-prefixes "geiser-autodoc" '("geiser-autodoc-"))

;;;***

;;;### (autoloads nil "geiser-base" "geiser-base.el" (0 0 0 0))
;;; Generated autoloads from geiser-base.el

(register-definition-prefixes "geiser-base" '("geiser--"))

;;;***

;;;### (autoloads nil "geiser-company" "geiser-company.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from geiser-company.el

(register-definition-prefixes "geiser-company" '("geiser-company--"))

;;;***

;;;### (autoloads nil "geiser-compile" "geiser-compile.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from geiser-compile.el

(register-definition-prefixes "geiser-compile" '("geiser-"))

;;;***

;;;### (autoloads nil "geiser-completion" "geiser-completion.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from geiser-completion.el

(register-definition-prefixes "geiser-completion" '("geiser-"))

;;;***

;;;### (autoloads nil "geiser-connection" "geiser-connection.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from geiser-connection.el

(register-definition-prefixes "geiser-connection" '("geiser-con"))

;;;***

;;;### (autoloads nil "geiser-custom" "geiser-custom.el" (0 0 0 0))
;;; Generated autoloads from geiser-custom.el

(register-definition-prefixes "geiser-custom" '("geiser-custom-"))

;;;***

;;;### (autoloads nil "geiser-debug" "geiser-debug.el" (0 0 0 0))
;;; Generated autoloads from geiser-debug.el

(register-definition-prefixes "geiser-debug" '("geiser-debug-"))

;;;***

;;;### (autoloads nil "geiser-doc" "geiser-doc.el" (0 0 0 0))
;;; Generated autoloads from geiser-doc.el

(register-definition-prefixes "geiser-doc" '("geiser-doc-"))

;;;***

;;;### (autoloads nil "geiser-edit" "geiser-edit.el" (0 0 0 0))
;;; Generated autoloads from geiser-edit.el

(register-definition-prefixes "geiser-edit" '("geiser-"))

;;;***

;;;### (autoloads nil "geiser-eval" "geiser-eval.el" (0 0 0 0))
;;; Generated autoloads from geiser-eval.el

(register-definition-prefixes "geiser-eval" '("geiser-eval--"))

;;;***

;;;### (autoloads nil "geiser-image" "geiser-image.el" (0 0 0 0))
;;; Generated autoloads from geiser-image.el

(register-definition-prefixes "geiser-image" '("geiser-"))

;;;***

;;;### (autoloads nil "geiser-impl" "geiser-impl.el" (0 0 0 0))
;;; Generated autoloads from geiser-impl.el

(register-definition-prefixes "geiser-impl" '("define-geiser-implementation" "geiser-" "with--geiser-implementation"))

;;;***

;;;### (autoloads nil "geiser-log" "geiser-log.el" (0 0 0 0))
;;; Generated autoloads from geiser-log.el

(register-definition-prefixes "geiser-log" '("geiser-"))

;;;***

;;;### (autoloads nil "geiser-menu" "geiser-menu.el" (0 0 0 0))
;;; Generated autoloads from geiser-menu.el

(register-definition-prefixes "geiser-menu" '("geiser-menu--"))

;;;***

;;;### (autoloads nil "geiser-mode" "geiser-mode.el" (0 0 0 0))
;;; Generated autoloads from geiser-mode.el

(register-definition-prefixes "geiser-mode" '("geiser-" "turn-o"))

;;;***

;;;### (autoloads nil "geiser-popup" "geiser-popup.el" (0 0 0 0))
;;; Generated autoloads from geiser-popup.el

(register-definition-prefixes "geiser-popup" '("geiser-popup-"))

;;;***

;;;### (autoloads nil "geiser-reload" "geiser-reload.el" (0 0 0 0))
;;; Generated autoloads from geiser-reload.el

(register-definition-prefixes "geiser-reload" '("geiser-"))

;;;***

;;;### (autoloads nil "geiser-repl" "geiser-repl.el" (0 0 0 0))
;;; Generated autoloads from geiser-repl.el

(register-definition-prefixes "geiser-repl" '("geiser" "run-geiser" "switch-to-geiser"))

;;;***

;;;### (autoloads nil "geiser-syntax" "geiser-syntax.el" (0 0 0 0))
;;; Generated autoloads from geiser-syntax.el

(register-definition-prefixes "geiser-syntax" '("geiser-syntax--"))

;;;***

;;;### (autoloads nil "geiser-table" "geiser-table.el" (0 0 0 0))
;;; Generated autoloads from geiser-table.el

(register-definition-prefixes "geiser-table" '("geiser-table-"))

;;;***

;;;### (autoloads nil "geiser-xref" "geiser-xref.el" (0 0 0 0))
;;; Generated autoloads from geiser-xref.el

(register-definition-prefixes "geiser-xref" '("geiser-xref-"))

;;;***

;;;### (autoloads nil nil ("geiser-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; geiser-autoloads.el ends here
;;; git-commit-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "git-commit" "git-commit.el" (0 0 0 0))
;;; Generated autoloads from git-commit.el
(put 'git-commit-major-mode 'safe-local-variable
    (lambda (val)
      (memq val '(text-mode
                  markdown-mode
                  org-mode
                  fundamental-mode
                  git-commit-elisp-text-mode))))

(register-definition-prefixes "git-commit" '("git-commit-" "global-git-commit-mode"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; git-commit-autoloads.el ends here
;;; git-timemachine-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "git-timemachine" "git-timemachine.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from git-timemachine.el

(autoload 'git-timemachine-toggle "git-timemachine" "\
Toggle git timemachine mode." t nil)

(autoload 'git-timemachine "git-timemachine" "\
Enable git timemachine for file of current buffer." t nil)

(autoload 'git-timemachine-switch-branch "git-timemachine" "\
Enable git timemachine for current buffer, switching to GIT-BRANCH.

\(fn GIT-BRANCH)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-timemachine" '("git-timemachine-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; git-timemachine-autoloads.el ends here
;;; gitconfig-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gitconfig-mode" "gitconfig-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from gitconfig-mode.el

(autoload 'gitconfig-mode "gitconfig-mode" "\
A major mode for editing .gitconfig files.

\(fn)" t nil)

(dolist (pattern '("/\\.gitconfig\\'" "/\\.git/config\\'" "/modules/.*/config\\'" "/git/config\\'" "/\\.gitmodules\\'" "/etc/gitconfig\\'")) (add-to-list 'auto-mode-alist (cons pattern 'gitconfig-mode)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitconfig-mode" '("gitconfig-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gitconfig-mode-autoloads.el ends here
;;; gnu-elpa-keyring-update-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gnu-elpa-keyring-update" "gnu-elpa-keyring-update.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from gnu-elpa-keyring-update.el

(defvar gnu-elpa-keyring-update--keyring (let ((kr (expand-file-name "etc/gnu-elpa.gpg-keyring" (file-name-directory load-file-name)))) (if (and load-file-name (file-readable-p kr)) kr "etc/gnu-elpa.gpg-keyring")))

(autoload 'gnu-elpa-keyring-update "gnu-elpa-keyring-update" "\
Import new GNU ELPA keys (if any) into package.el's keyring." nil nil)
 (eval-after-load 'package
  `(and (bound-and-true-p package-user-dir)
        (file-directory-p package-user-dir)
        (let ((ts (expand-file-name
                   "gnu-elpa.timestamp"
                   (or (bound-and-true-p package-gnupghome-dir)
                       (expand-file-name "gnupg"
                                         package-user-dir))))
              (kr gnu-elpa-keyring-update--keyring))
          (and (file-writable-p ts)
               (file-readable-p kr)
               (file-newer-than-file-p kr ts)
               (gnu-elpa-keyring-update)))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-elpa-keyring-update" '("gnu-elpa-keyring-update--keyring")))

;;;***

;;;### (autoloads nil nil ("gnu-elpa-keyring-update-pkg.el") (0 0
;;;;;;  0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gnu-elpa-keyring-update-autoloads.el ends here
;;; go-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "go-mode" "go-mode.el" (0 0 0 0))
;;; Generated autoloads from go-mode.el

(autoload 'go-mode "go-mode" "\
Major mode for editing Go source text.

This mode provides (not just) basic editing capabilities for
working with Go code. It offers almost complete syntax
highlighting, indentation that is almost identical to gofmt and
proper parsing of the buffer content to allow features such as
navigation by function, manipulation of comments or detection of
strings.

In addition to these core features, it offers various features to
help with writing Go code. You can directly run buffer content
through gofmt, read godoc documentation from within Emacs, modify
and clean up the list of package imports or interact with the
Playground (uploading and downloading pastes).

The following extra functions are defined:

- `gofmt'
- `godoc' and `godoc-at-point'
- `go-import-add'
- `go-remove-unused-imports'
- `go-goto-arguments'
- `go-goto-docstring'
- `go-goto-function'
- `go-goto-function-name'
- `go-goto-imports'
- `go-goto-return-values'
- `go-goto-method-receiver'
- `go-play-buffer' and `go-play-region'
- `go-download-play'
- `godef-describe' and `godef-jump'
- `go-coverage'
- `go-set-project'
- `go-reset-gopath'

If you want to automatically run `gofmt' before saving a file,
add the following hook to your emacs configuration:

\(add-hook 'before-save-hook #'gofmt-before-save)

If you want to use `godef-jump' instead of etags (or similar),
consider binding godef-jump to `M-.', which is the default key
for `find-tag':

\(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd \"M-.\") #'godef-jump)))

Please note that godef is an external dependency. You can install
it with

go get github.com/rogpeppe/godef


If you're looking for even more integration with Go, namely
on-the-fly syntax checking, auto-completion and snippets, it is
recommended that you look at flycheck
\(see URL `https://github.com/flycheck/flycheck') or flymake in combination
with goflymake (see URL `https://github.com/dougm/goflymake'), gocode
\(see URL `https://github.com/nsf/gocode'), go-eldoc
\(see URL `github.com/syohex/emacs-go-eldoc') and yasnippet-go
\(see URL `https://github.com/dominikh/yasnippet-go')

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-mode))

(autoload 'gofmt-before-save "go-mode" "\
Add this to .emacs to run gofmt on the current buffer when saving:
\(add-hook 'before-save-hook 'gofmt-before-save).

Note that this will cause ‘go-mode’ to get loaded the first time
you save any file, kind of defeating the point of autoloading." t nil)

(autoload 'godoc "go-mode" "\
Show Go documentation for QUERY, much like \\<go-mode-map>\\[man].

\(fn QUERY)" t nil)

(autoload 'go-download-play "go-mode" "\
Download a paste from the playground and insert it in a Go buffer.
Tries to look for a URL at point.

\(fn URL)" t nil)

(autoload 'go-dot-mod-mode "go-mode" "\
A major mode for editing go.mod files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-dot-mod-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "go-mode" '("go-" "god" "gofmt")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; go-mode-autoloads.el ends here
;;; goto-chg-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "goto-chg" "goto-chg.el" (0 0 0 0))
;;; Generated autoloads from goto-chg.el

(autoload 'goto-last-change "goto-chg" "\
Go to the point where the last edit was made in the current buffer.
Repeat the command to go to the second last edit, etc.

To go back to more recent edit, the reverse of this command, use \\[goto-last-change-reverse]
or precede this command with \\[universal-argument] - (minus).

It does not go to the same point twice even if there has been many edits
there. I call the minimal distance between distinguishable edits \"span\".
Set variable `glc-default-span' to control how close is \"the same point\".
Default span is 8.
The span can be changed temporarily with \\[universal-argument] right before \\[goto-last-change]:
\\[universal-argument] <NUMBER> set current span to that number,
\\[universal-argument] (no number) multiplies span by 4, starting with default.
The so set span remains until it is changed again with \\[universal-argument], or the consecutive
repetition of this command is ended by any other command.

When span is zero (i.e. \\[universal-argument] 0) subsequent \\[goto-last-change] visits each and
every point of edit and a message shows what change was made there.
In this case it may go to the same point twice.

This command uses undo information. If undo is disabled, so is this command.
At times, when undo information becomes too large, the oldest information is
discarded. See variable `undo-limit'.

\(fn ARG)" t nil)

(autoload 'goto-last-change-reverse "goto-chg" "\
Go back to more recent changes after \\[goto-last-change] have been used.
See `goto-last-change' for use of prefix argument.

\(fn ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "goto-chg" '("glc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; goto-chg-autoloads.el ends here
;;; groovy-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "groovy-electric" "groovy-electric.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from groovy-electric.el

(autoload 'groovy-electric-mode "groovy-electric" "\
Toggle Groovy Electric minor mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode.

If called interactively, enable Groovy-Electric mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

When Groovy Electric mode is enabled, simple, double and back
quotes as well as braces are paired auto-magically. Expansion
does not occur inside comments and strings. Note that you must
have Font Lock enabled. ${ } is expanded when in a GString

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "groovy-electric" '("groovy-electric-")))

;;;***

;;;### (autoloads nil "groovy-mode" "groovy-mode.el" (0 0 0 0))
;;; Generated autoloads from groovy-mode.el

(add-to-list 'auto-mode-alist '("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" . groovy-mode))

(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))

(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(autoload 'groovy-mode "groovy-mode" "\
Major mode for editing Groovy code.

The hook `groovy-mode-hook' is run with no args at mode
initialization.

Key bindings:
\\{groovy-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "groovy-mode" '("groovy-")))

;;;***

;;;### (autoloads nil "inf-groovy" "inf-groovy.el" (0 0 0 0))
;;; Generated autoloads from inf-groovy.el

(autoload 'inf-groovy-keys "inf-groovy" "\
Set local key defs for inf-groovy in groovy-mode" nil nil)

(autoload 'inferior-groovy-mode "inf-groovy" "\
Major mode for interacting with an inferior groovy (groovysh) process.

The following commands are available:
\\{inferior-groovy-mode-map}

A groovy process can be fired up with M-x run-groovy.

Customisation: Entry to this mode runs the hooks on comint-mode-hook and
inferior-groovy-mode-hook (in that order).

You can send text to the inferior groovy process from other buffers containing
Groovy source.
    switch-to-groovy switches the current buffer to the groovy process buffer.
    groovy-send-definition sends the current definition to the groovy process.
    groovy-send-region sends the current region to the groovy process.

    groovy-send-definition-and-go, groovy-send-region-and-go,
        switch to the groovy process buffer after sending their text.
For information on running multiple processes in multiple buffers, see
documentation for variable groovy-buffer.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for groovy; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  # start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it." t nil)

(autoload 'run-groovy "inf-groovy" "\
Run an inferior Groovy process, input and output via buffer *groovy*.
If there is a process already running in *groovy*, switch to that buffer.
With a prefix argument, prompt for the groovysh path and arguments
\(see variables `groovysh' and `groovysh-args' for the defaults).

Runs the hook `inferior-groovy-mode-hook' (after the
`comint-mode-hook' is run).  Type \\[describe-mode] in the
process buffer for a list of commands.

\(fn CMD)" t nil)

(eval-after-load 'groovy-mode (lambda nil (add-hook 'groovy-mode-hook 'inf-groovy-keys)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "inf-groovy" '("groovy" "inf" "remove-in-string" "switch-to-groovy")))

;;;***

;;;### (autoloads nil nil ("groovy-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; groovy-mode-autoloads.el ends here
;;; haml-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "haml-mode" "haml-mode.el" (0 0 0 0))
;;; Generated autoloads from haml-mode.el

(autoload 'haml-mode "haml-mode" "\
Major mode for editing Haml files.

\\{haml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haml-mode" '("haml-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; haml-mode-autoloads.el ends here
;;; haskell-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ghc-core" "ghc-core.el" (0 0 0 0))
;;; Generated autoloads from ghc-core.el

(let ((loads (get 'ghc-core 'custom-loads))) (if (member '"ghc-core" loads) nil (put 'ghc-core 'custom-loads (cons '"ghc-core" loads))))

(autoload 'ghc-core-create-core "ghc-core" "\
Compile and load the current buffer as tidy core." t nil)

(add-to-list 'auto-mode-alist '("\\.hcr\\'" . ghc-core-mode))

(add-to-list 'auto-mode-alist '("\\.dump-simpl\\'" . ghc-core-mode))

(autoload 'ghc-core-mode "ghc-core" "\
Major mode for GHC Core files.

\(fn)" t nil)

(register-definition-prefixes "ghc-core" '("ghc-core-"))

;;;***

;;;### (autoloads nil "ghci-script-mode" "ghci-script-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ghci-script-mode.el

(autoload 'ghci-script-mode "ghci-script-mode" "\
Major mode for working with .ghci files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.ghci\\'" . ghci-script-mode))

(register-definition-prefixes "ghci-script-mode" '("ghci-script-mode-"))

;;;***

;;;### (autoloads nil "haskell" "haskell.el" (0 0 0 0))
;;; Generated autoloads from haskell.el

(autoload 'interactive-haskell-mode "haskell" "\
Minor mode for enabling haskell-process interaction.

If called interactively, toggle `Interactive-Haskell mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'haskell-interactive-mode-return "haskell" "\
Handle the return key." t nil)

(autoload 'haskell-session-kill "haskell" "\
Kill the session process and buffer, delete the session.
1. Kill the process.
2. Kill the interactive buffer unless LEAVE-INTERACTIVE-BUFFER is not given.
3. Walk through all the related buffers and set their haskell-session to nil.
4. Remove the session from the sessions list.

\(fn &optional LEAVE-INTERACTIVE-BUFFER)" t nil)

(autoload 'haskell-interactive-kill "haskell" "\
Kill the buffer and (maybe) the session." t nil)

(autoload 'haskell-session "haskell" "\
Get the Haskell session, prompt if there isn't one or fail." nil nil)

(autoload 'haskell-interactive-switch "haskell" "\
Switch to the interactive mode for this session." t nil)

(autoload 'haskell-session-change "haskell" "\
Change the session for the current buffer." t nil)

(autoload 'haskell-kill-session-process "haskell" "\
Kill the process.

\(fn &optional SESSION)" t nil)

(autoload 'haskell-interactive-mode-visit-error "haskell" "\
Visit the buffer of the current (or last) error message." t nil)

(autoload 'haskell-mode-jump-to-tag "haskell" "\
Jump to the tag of the given identifier.

Give optional NEXT-P parameter to override value of
`xref-prompt-for-identifier' during definition search.

\(fn &optional NEXT-P)" t nil)

(autoload 'haskell-mode-after-save-handler "haskell" "\
Function that will be called after buffer's saving." nil nil)

(autoload 'haskell-mode-tag-find "haskell" "\
The tag find function, specific for the particular session.

\(fn &optional NEXT-P)" t nil)

(autoload 'haskell-interactive-bring "haskell" "\
Bring up the interactive mode for this session." t nil)

(autoload 'haskell-process-load-file "haskell" "\
Load the current buffer file." t nil)

(autoload 'haskell-process-reload "haskell" "\
Re-load the current buffer file." t nil)

(autoload 'haskell-process-reload-file "haskell" nil nil nil)

(autoload 'haskell-process-load-or-reload "haskell" "\
Load or reload. Universal argument toggles which.

\(fn &optional TOGGLE)" t nil)

(autoload 'haskell-process-cabal-build "haskell" "\
Build the Cabal project." t nil)

(autoload 'haskell-process-cabal "haskell" "\
Prompts for a Cabal command to run.

\(fn P)" t nil)

(autoload 'haskell-process-minimal-imports "haskell" "\
Dump minimal imports." t nil)

(register-definition-prefixes "haskell" '("haskell-" "interactive-haskell-mode-map" "xref-prompt-for-identifier"))

;;;***

;;;### (autoloads nil "haskell-align-imports" "haskell-align-imports.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-align-imports.el

(autoload 'haskell-align-imports "haskell-align-imports" "\
Align all the imports in the buffer." t nil)

(register-definition-prefixes "haskell-align-imports" '("haskell-align-imports-"))

;;;***

;;;### (autoloads nil "haskell-c2hs" "haskell-c2hs.el" (0 0 0 0))
;;; Generated autoloads from haskell-c2hs.el

(add-to-list 'auto-mode-alist '("\\.chs\\'" . haskell-c2hs-mode))

(autoload 'haskell-c2hs-mode "haskell-c2hs" "\
Mode for editing *.chs files of the c2hs haskell tool.

\(fn)" t nil)

(register-definition-prefixes "haskell-c2hs" '("haskell-c2hs-font-lock-keywords"))

;;;***

;;;### (autoloads nil "haskell-cabal" "haskell-cabal.el" (0 0 0 0))
;;; Generated autoloads from haskell-cabal.el

(add-to-list 'auto-mode-alist '("\\.cabal\\'\\|/cabal\\.project\\|/\\.cabal/config\\'" . haskell-cabal-mode))

(autoload 'haskell-cabal-mode "haskell-cabal" "\
Major mode for Cabal package description files.

\(fn)" t nil)

(autoload 'haskell-cabal-get-field "haskell-cabal" "\
Read the value of field with NAME from project's cabal file.
If there is no valid .cabal file to get the setting from (or
there is no corresponding setting with that name in the .cabal
file), then this function returns nil.

\(fn NAME)" t nil)

(autoload 'haskell-cabal-get-dir "haskell-cabal" "\
Get the Cabal dir for a new project. Various ways of figuring this out,
   and indeed just prompting the user. Do them all.

\(fn &optional USE-DEFAULTS)" nil nil)

(autoload 'haskell-cabal-visit-file "haskell-cabal" "\
Locate and visit package description file for file visited by current buffer.
This uses `haskell-cabal-find-file' to locate the closest
\".cabal\" file and open it.  This command assumes a common Cabal
project structure where the \".cabal\" file is in the top-folder
of the project, and all files related to the project are in or
below the top-folder.  If called with non-nil prefix argument
OTHER-WINDOW use `find-file-other-window'.

\(fn OTHER-WINDOW)" t nil)

(let ((loads (get 'haskell-cabal 'custom-loads))) (if (member '"haskell-cabal" loads) nil (put 'haskell-cabal 'custom-loads (cons '"haskell-cabal" loads))))

(register-definition-prefixes "haskell-cabal" '("haskell-"))

;;;***

;;;### (autoloads nil "haskell-collapse" "haskell-collapse.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from haskell-collapse.el

(autoload 'haskell-collapse-mode "haskell-collapse" "\
Minor mode to collapse and expand haskell expressions

If called interactively, toggle `Haskell-Collapse mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "haskell-collapse" '("haskell-"))

;;;***

;;;### (autoloads nil "haskell-commands" "haskell-commands.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from haskell-commands.el

(autoload 'haskell-process-restart "haskell-commands" "\
Restart the inferior Haskell process." t nil)

(autoload 'haskell-process-clear "haskell-commands" "\
Clear the current process." t nil)

(autoload 'haskell-process-interrupt "haskell-commands" "\
Interrupt the process (SIGINT)." t nil)

(autoload 'haskell-describe "haskell-commands" "\
Describe the given identifier IDENT.

\(fn IDENT)" t nil)

(autoload 'haskell-rgrep "haskell-commands" "\
Grep the effective project for the symbol at point.
Very useful for codebase navigation.

Prompts for an arbitrary regexp given a prefix arg PROMPT.

\(fn &optional PROMPT)" t nil)

(autoload 'haskell-process-do-info "haskell-commands" "\
Print info on the identifier at point.
If PROMPT-VALUE is non-nil, request identifier via mini-buffer.

\(fn &optional PROMPT-VALUE)" t nil)

(autoload 'haskell-process-do-type "haskell-commands" "\
Print the type of the given expression.

Given INSERT-VALUE prefix indicates that result type signature
should be inserted.

\(fn &optional INSERT-VALUE)" t nil)

(autoload 'haskell-mode-jump-to-def-or-tag "haskell-commands" "\
Jump to the definition.
Jump to definition of identifier at point by consulting GHCi, or
tag table as fallback.

Remember: If GHCi is busy doing something, this will delay, but
it will always be accurate, in contrast to tags, which always
work but are not always accurate.
If the definition or tag is found, the location from which you jumped
will be pushed onto `xref--marker-ring', so you can return to that
position with `xref-pop-marker-stack'.

\(fn &optional NEXT-P)" t nil)

(autoload 'haskell-mode-goto-loc "haskell-commands" "\
Go to the location of the thing at point.
Requires the :loc-at command from GHCi." t nil)

(autoload 'haskell-mode-jump-to-def "haskell-commands" "\
Jump to definition of identifier IDENT at point.

\(fn IDENT)" t nil)

(autoload 'haskell-process-cd "haskell-commands" "\
Change directory.

\(fn &optional NOT-INTERACTIVE)" t nil)

(autoload 'haskell-process-cabal-macros "haskell-commands" "\
Send the cabal macros string." t nil)

(autoload 'haskell-mode-show-type-at "haskell-commands" "\
Show type of the thing at point or within active region asynchronously.
This function requires GHCi 8+ or GHCi-ng.

\\<haskell-interactive-mode-map>
To make this function works sometimes you need to load the file in REPL
first using command `haskell-process-load-file' bound to
\\[haskell-process-load-file].

Optional argument INSERT-VALUE indicates that
recieved type signature should be inserted (but only if nothing
happened since function invocation).

\(fn &optional INSERT-VALUE)" t nil)

(autoload 'haskell-process-unignore "haskell-commands" "\
Unignore any ignored files.
Do not ignore files that were specified as being ignored by the
inferior GHCi process." t nil)

(autoload 'haskell-session-change-target "haskell-commands" "\
Set the build TARGET for cabal REPL.

\(fn TARGET)" t nil)

(autoload 'haskell-mode-stylish-buffer "haskell-commands" "\
Apply stylish-haskell to the current buffer.

Use `haskell-mode-stylish-haskell-path' to know where to find
stylish-haskell executable.  This function tries to preserve
cursor position and markers by using
`haskell-mode-buffer-apply-command'." t nil)

(autoload 'haskell-mode-find-uses "haskell-commands" "\
Find use cases of the identifier at point and highlight them all." t nil)

(register-definition-prefixes "haskell-commands" '("haskell-"))

;;;***

;;;### (autoloads nil "haskell-compile" "haskell-compile.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from haskell-compile.el

(let ((loads (get 'haskell-compile 'custom-loads))) (if (member '"haskell-compile" loads) nil (put 'haskell-compile 'custom-loads (cons '"haskell-compile" loads))))

(autoload 'haskell-compile "haskell-compile" "\
Run a compile command for the current Haskell buffer.
Obeys haskell-compiler-type to choose the appropriate build command.

If prefix argument EDIT-COMMAND is non-nil (and not a negative
prefix `-'), prompt for a custom compile command.

If EDIT-COMMAND contains the negative prefix argument `-', call
the alternative command defined in
`haskell-compile-stack-build-alt-command' /
`haskell-compile-cabal-build-alt-command'.

If there is no prefix argument, the most recent custom compile
command is used, falling back to
`haskell-compile-stack-build-command' for stack builds
`haskell-compile-cabal-build-command' for cabal builds, and
`haskell-compile-command' otherwise.

'% characters in the `-command' templates are replaced by the
base directory for build tools, or the current buffer for
`haskell-compile-command'.

\(fn &optional EDIT-COMMAND)" t nil)

(register-definition-prefixes "haskell-compile" '("haskell-"))

;;;***

;;;### (autoloads nil "haskell-complete-module" "haskell-complete-module.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-complete-module.el

(register-definition-prefixes "haskell-complete-module" '("haskell-complete-module"))

;;;***

;;;### (autoloads nil "haskell-completions" "haskell-completions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-completions.el

(let ((loads (get 'haskell-completions 'custom-loads))) (if (member '"haskell-completions" loads) nil (put 'haskell-completions 'custom-loads (cons '"haskell-completions" loads))))

(autoload 'haskell-completions-completion-at-point "haskell-completions" "\
Provide completion list for thing at point.
This function is used in non-interactive `haskell-mode'.  It
provides completions for haskell keywords, language pragmas,
GHC's options, and language extensions, but not identifiers." nil nil)

(register-definition-prefixes "haskell-completions" '("haskell-completions-"))

;;;***

;;;### (autoloads nil "haskell-customize" "haskell-customize.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-customize.el

(let ((loads (get 'haskell 'custom-loads))) (if (member '"haskell-customize" loads) nil (put 'haskell 'custom-loads (cons '"haskell-customize" loads))))

(let ((loads (get 'haskell-interactive 'custom-loads))) (if (member '"haskell-customize" loads) nil (put 'haskell-interactive 'custom-loads (cons '"haskell-customize" loads))))

(register-definition-prefixes "haskell-customize" '("haskell-" "inferior-haskell-root-dir"))

;;;***

;;;### (autoloads nil "haskell-debug" "haskell-debug.el" (0 0 0 0))
;;; Generated autoloads from haskell-debug.el

(let ((loads (get 'haskell-debug 'custom-loads))) (if (member '"haskell-debug" loads) nil (put 'haskell-debug 'custom-loads (cons '"haskell-debug" loads))))

(defface haskell-debug-warning-face '((t :inherit 'compilation-warning)) "\
Face for warnings." :group 'haskell-debug)

(defface haskell-debug-trace-number-face '((t :weight bold :background "#f5f5f5")) "\
Face for numbers in backtrace." :group 'haskell-debug)

(defface haskell-debug-newline-face '((t :weight bold :background "#f0f0f0")) "\
Face for newlines in trace steps." :group 'haskell-debug)

(defface haskell-debug-keybinding-face '((t :inherit 'font-lock-type-face :weight bold)) "\
Face for keybindings." :group 'haskell-debug)

(defface haskell-debug-heading-face '((t :inherit 'font-lock-keyword-face)) "\
Face for headings." :group 'haskell-debug)

(defface haskell-debug-muted-face '((t :foreground "#999")) "\
Face for muteds." :group 'haskell-debug)

(register-definition-prefixes "haskell-debug" '("haskell-debug"))

;;;***

;;;### (autoloads nil "haskell-decl-scan" "haskell-decl-scan.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-decl-scan.el

(let ((loads (get 'haskell-decl-scan 'custom-loads))) (if (member '"haskell-decl-scan" loads) nil (put 'haskell-decl-scan 'custom-loads (cons '"haskell-decl-scan" loads))))

(autoload 'haskell-ds-create-imenu-index "haskell-decl-scan" "\
Function for finding `imenu' declarations in Haskell mode.
Finds all declarations (classes, variables, imports, instances and
datatypes) in a Haskell file for the `imenu' package." nil nil)

(autoload 'turn-on-haskell-decl-scan "haskell-decl-scan" "\
Unconditionally activate `haskell-decl-scan-mode'." t nil)

(autoload 'haskell-decl-scan-mode "haskell-decl-scan" "\
Toggle Haskell declaration scanning minor mode on or off.
With a prefix argument ARG, enable minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

See also info node `(haskell-mode)haskell-decl-scan-mode' for
more details about this minor mode.

Top-level declarations are scanned and listed in the menu item
\"Declarations\" (if enabled via option
`haskell-decl-scan-add-to-menubar').  Selecting an item from this
menu will take point to the start of the declaration.

\\[beginning-of-defun] and \\[end-of-defun] move forward and backward to the start of a declaration.

This may link with `haskell-doc-mode'.

For non-literate and LaTeX-style literate scripts, we assume the
common convention that top-level declarations start at the first
column.  For Bird-style literate scripts, we assume the common
convention that top-level declarations start at the third column,
ie. after \"> \".

Anything in `font-lock-comment-face' is not considered for a
declaration.  Therefore, using Haskell font locking with comments
coloured in `font-lock-comment-face' improves declaration scanning.

Literate Haskell scripts are supported: If the value of
`haskell-literate' (set automatically by `haskell-literate-mode')
is `bird', a Bird-style literate script is assumed.  If it is nil
or `tex', a non-literate or LaTeX-style literate script is
assumed, respectively.

Invokes `haskell-decl-scan-mode-hook' on activation.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "haskell-decl-scan" '("haskell-d" "literate-haskell-ds-"))

;;;***

;;;### (autoloads nil "haskell-doc" "haskell-doc.el" (0 0 0 0))
;;; Generated autoloads from haskell-doc.el

(let ((loads (get 'haskell-doc 'custom-loads))) (if (member '"haskell-doc" loads) nil (put 'haskell-doc 'custom-loads (cons '"haskell-doc" loads))))

(autoload 'haskell-doc-mode "haskell-doc" "\
Enter `haskell-doc-mode' for showing fct types in the echo area.
See variable docstring.

\(fn &optional ARG)" t nil)

(defalias 'turn-on-haskell-doc-mode 'haskell-doc-mode)

(defalias 'turn-on-haskell-doc 'haskell-doc-mode)

(autoload 'haskell-doc-current-info "haskell-doc" "\
Return the info about symbol at point.
Meant for `eldoc-documentation-function'." nil nil)

(autoload 'haskell-doc-show-type "haskell-doc" "\
Show the type of the function near point or given symbol SYM.
For the function under point, show the type in the echo area.
This information is extracted from the `haskell-doc-prelude-types' alist
of prelude functions and their types, or from the local functions in the
current buffer.

\(fn &optional SYM)" t nil)

(register-definition-prefixes "haskell-doc" '("haskell-" "inferior-haskell-" "turn-off-haskell-doc"))

;;;***

;;;### (autoloads nil "haskell-font-lock" "haskell-font-lock.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-font-lock.el

(let ((loads (get 'haskell-appearance 'custom-loads))) (if (member '"haskell-font-lock" loads) nil (put 'haskell-appearance 'custom-loads (cons '"haskell-font-lock" loads))))

(defface haskell-keyword-face '((t :inherit font-lock-keyword-face)) "\
Face used to highlight Haskell keywords." :group 'haskell-appearance)

(defface haskell-type-face '((t :inherit font-lock-type-face)) "\
Face used to highlight Haskell types" :group 'haskell-appearance)

(defface haskell-constructor-face '((t :inherit font-lock-type-face)) "\
Face used to highlight Haskell constructors." :group 'haskell-appearance)

(defface haskell-operator-face '((t :inherit font-lock-variable-name-face)) "\
Face used to highlight Haskell operators." :group 'haskell-appearance)

(defface haskell-pragma-face '((t :inherit font-lock-preprocessor-face)) "\
Face used to highlight Haskell pragmas ({-# ... #-})." :group 'haskell-appearance)

(defface haskell-liquid-haskell-annotation-face '((t :inherit haskell-pragma-face)) "\
Face used to highlight LiquidHaskell annotations ({-@ ... @-})." :group 'haskell-appearance)

(defface haskell-literate-comment-face '((t :inherit font-lock-doc-face)) "\
Face with which to fontify literate comments.
Inherit from `default' to avoid fontification of them." :group 'haskell-appearance)

(register-definition-prefixes "haskell-font-lock" '("haskell-"))

;;;***

;;;### (autoloads nil "haskell-ghc-support" "haskell-ghc-support.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-ghc-support.el

(register-definition-prefixes "haskell-ghc-support" '("haskell-"))

;;;***

;;;### (autoloads nil "haskell-hoogle" "haskell-hoogle.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from haskell-hoogle.el

(autoload 'haskell-hoogle "haskell-hoogle" "\
Do a Hoogle search for QUERY.

If prefix argument INFO is given, then `haskell-hoogle-command'
is asked to show extra info for the items matching QUERY..

\(fn QUERY &optional INFO)" t nil)

(defalias 'hoogle 'haskell-hoogle)

(autoload 'haskell-hoogle-lookup-from-website "haskell-hoogle" "\
Lookup QUERY at `haskell-hoogle-url'.

\(fn QUERY)" t nil)

(autoload 'haskell-hoogle-lookup-from-local "haskell-hoogle" "\
Lookup QUERY on local hoogle server." t nil)

(register-definition-prefixes "haskell-hoogle" '("haskell-hoogle-" "hoogle-prompt"))

;;;***

;;;### (autoloads nil "haskell-indent" "haskell-indent.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from haskell-indent.el

(let ((loads (get 'haskell-indent 'custom-loads))) (if (member '"haskell-indent" loads) nil (put 'haskell-indent 'custom-loads (cons '"haskell-indent" loads))))

(autoload 'turn-on-haskell-indent "haskell-indent" "\
Turn on ``intelligent'' Haskell indentation mode." nil nil)

(autoload 'haskell-indent-mode "haskell-indent" "\
``Intelligent'' Haskell indentation mode.
This deals with the layout rule of Haskell.
\\[haskell-indent-cycle] starts the cycle which proposes new
possibilities as long as the TAB key is pressed.  Any other key
or mouse click terminates the cycle and is interpreted except for
RET which merely exits the cycle.
Other special keys are:
    \\[haskell-indent-insert-equal]
      inserts an =
    \\[haskell-indent-insert-guard]
      inserts an |
    \\[haskell-indent-insert-otherwise]
      inserts an | otherwise =
these functions also align the guards and rhs of the current definition
    \\[haskell-indent-insert-where]
      inserts a where keyword
    \\[haskell-indent-align-guards-and-rhs]
      aligns the guards and rhs of the region
    \\[haskell-indent-put-region-in-literate]
      makes the region a piece of literate code in a literate script

If `ARG' is falsey, toggle `haskell-indent-mode'.  Else sets
`haskell-indent-mode' to whether `ARG' is greater than 0.

Invokes `haskell-indent-hook' if not nil.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "haskell-indent" '("haskell-indent-" "turn-off-haskell-indent"))

;;;***

;;;### (autoloads nil "haskell-indentation" "haskell-indentation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-indentation.el

(let ((loads (get 'haskell-indentation 'custom-loads))) (if (member '"haskell-indentation" loads) nil (put 'haskell-indentation 'custom-loads (cons '"haskell-indentation" loads))))

(autoload 'haskell-indentation-mode "haskell-indentation" "\
Haskell indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.

If called interactively, toggle `Haskell-Indentation mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-haskell-indentation "haskell-indentation" "\
Turn on the haskell-indentation minor mode." t nil)

(register-definition-prefixes "haskell-indentation" '("haskell-indentation-"))

;;;***

;;;### (autoloads nil "haskell-interactive-mode" "haskell-interactive-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-interactive-mode.el

(defface haskell-interactive-face-prompt '((t :inherit font-lock-function-name-face)) "\
Face for the prompt." :group 'haskell-interactive)

(defface haskell-interactive-face-prompt-cont '((t :inherit font-lock-keyword-face)) "\
Face for GHCi's prompt-cont in multi-line mode." :group 'haskell-interactive)

(define-obsolete-face-alias 'haskell-interactive-face-prompt2 'haskell-interactive-face-prompt-cont "16.2")

(defface haskell-interactive-face-compile-error '((t :inherit compilation-error)) "\
Face for compile errors." :group 'haskell-interactive)

(defface haskell-interactive-face-compile-warning '((t :inherit compilation-warning)) "\
Face for compiler warnings." :group 'haskell-interactive)

(defface haskell-interactive-face-result '((t :inherit font-lock-string-face)) "\
Face for the result." :group 'haskell-interactive)

(defface haskell-interactive-face-garbage '((t :inherit font-lock-string-face)) "\
Face for trailing garbage after a command has completed." :group 'haskell-interactive)

(autoload 'haskell-interactive-mode-reset-error "haskell-interactive-mode" "\
Reset the error cursor position.

\(fn SESSION)" t nil)

(autoload 'haskell-interactive-mode-echo "haskell-interactive-mode" "\
Echo a read only piece of text before the prompt.

\(fn SESSION MESSAGE &optional MODE)" nil nil)

(autoload 'haskell-process-show-repl-response "haskell-interactive-mode" "\
Send LINE to the GHCi process and echo the result in some fashion.
Result will be printed in the minibuffer or presented using
function `haskell-presentation-present', depending on variable
`haskell-process-use-presentation-mode'.

\(fn LINE)" nil nil)

(register-definition-prefixes "haskell-interactive-mode" '("haskell-"))

;;;***

;;;### (autoloads nil "haskell-lexeme" "haskell-lexeme.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from haskell-lexeme.el

(register-definition-prefixes "haskell-lexeme" '("haskell-lexeme-"))

;;;***

;;;### (autoloads nil "haskell-load" "haskell-load.el" (0 0 0 0))
;;; Generated autoloads from haskell-load.el

(defface haskell-error-face '((((supports :underline (:style wave))) :underline (:style wave :color "#dc322f")) (t :inherit error)) "\
Face used for marking error lines." :group 'haskell-mode)

(defface haskell-warning-face '((((supports :underline (:style wave))) :underline (:style wave :color "#b58900")) (t :inherit warning)) "\
Face used for marking warning lines." :group 'haskell-mode)

(defface haskell-hole-face '((((supports :underline (:style wave))) :underline (:style wave :color "#6c71c4")) (t :inherit warning)) "\
Face used for marking hole lines." :group 'haskell-mode)

(autoload 'haskell-process-reload-devel-main "haskell-load" "\
Reload the module `DevelMain' and then run `DevelMain.update'.

This is for doing live update of the code of servers or GUI
applications.  Put your development version of the program in
`DevelMain', and define `update' to auto-start the program on a
new thread, and use the `foreign-store' package to access the
running context across :load/:reloads in GHCi." t nil)

(register-definition-prefixes "haskell-load" '("haskell-"))

;;;***

;;;### (autoloads nil "haskell-menu" "haskell-menu.el" (0 0 0 0))
;;; Generated autoloads from haskell-menu.el

(autoload 'haskell-menu "haskell-menu" "\
Launch the Haskell sessions menu." t nil)

(register-definition-prefixes "haskell-menu" '("haskell-menu-"))

;;;***

;;;### (autoloads nil "haskell-mode" "haskell-mode.el" (0 0 0 0))
;;; Generated autoloads from haskell-mode.el

(autoload 'haskell-version "haskell-mode" "\
Show the `haskell-mode` version in the echo area.
With prefix argument HERE, insert it at point.

\(fn &optional HERE)" t nil)

(autoload 'haskell-mode-view-news "haskell-mode" "\
Display information on recent changes to haskell-mode." t nil)

(autoload 'haskell-mode "haskell-mode" "\
Major mode for editing Haskell programs.

\\<haskell-mode-map>

Literate Haskell scripts are supported via `haskell-literate-mode'.
The variable `haskell-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more details.

Use `haskell-version' to find out what version of Haskell mode you are
currently using.

Additional Haskell mode modules can be hooked in via `haskell-mode-hook'.

Indentation modes:

    `haskell-indentation-mode', Kristof Bastiaensen, Gergely Risko
      Intelligent semi-automatic indentation Mk2

    `haskell-indent-mode', Guy Lapalme
      Intelligent semi-automatic indentation.

Interaction modes:

    `interactive-haskell-mode'
      Interact with per-project GHCi processes through a REPL and
      directory-aware sessions.

Other modes:

    `haskell-decl-scan-mode', Graeme E Moss
      Scans top-level declarations, and places them in a menu.

    `haskell-doc-mode', Hans-Wolfgang Loidl
      Echoes types of functions or syntax of keywords when the cursor is idle.

To activate a minor-mode, simply run the interactive command. For
example, `M-x haskell-doc-mode'. Run it again to disable it.

To enable a mode for every haskell-mode buffer, add a hook in
your Emacs configuration. To do that you can customize
`haskell-mode-hook' or add lines to your .emacs file. For
example, to enable `interactive-haskell-mode', use the following:

    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

Minor modes that work well with `haskell-mode':

- `smerge-mode': show and work with diff3 conflict markers used
  by git, svn and other version control systems.

\(fn)" t nil)

(autoload 'haskell-forward-sexp "haskell-mode" "\
Haskell specific version of `forward-sexp'.

Move forward across one balanced expression (sexp).  With ARG, do
it that many times.  Negative arg -N means move backward across N
balanced expressions.  This command assumes point is not in a
string or comment.

If unable to move over a sexp, signal `scan-error' with three
arguments: a message, the start of the obstacle (a parenthesis or
list marker of some kind), and end of the obstacle.

\(fn &optional ARG)" t nil)

(autoload 'haskell-literate-mode "haskell-mode" "\
As `haskell-mode' but for literate scripts.

\(fn)" t nil)

(define-obsolete-function-alias 'literate-haskell-mode 'haskell-literate-mode "2020-04")

(add-to-list 'auto-mode-alist '("\\.[gh]s\\'" . haskell-mode))

(add-to-list 'auto-mode-alist '("\\.hsig\\'" . haskell-mode))

(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . haskell-literate-mode))

(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-mode))

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))

(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(add-to-list 'completion-ignored-extensions ".hi")

(autoload 'haskell-mode-generate-tags "haskell-mode" "\
Generate tags using Hasktags.  This is synchronous function.

If optional AND-THEN-FIND-THIS-TAG argument is present it is used
with function `xref-find-definitions' after new table was
generated.

\(fn &optional AND-THEN-FIND-THIS-TAG)" t nil)

(register-definition-prefixes "haskell-mode" '("haskell-"))

;;;***

;;;### (autoloads nil "haskell-modules" "haskell-modules.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from haskell-modules.el

(autoload 'haskell-session-installed-modules "haskell-modules" "\
Get the modules installed in the current package set.

\(fn SESSION &optional DONTCREATE)" nil nil)

(autoload 'haskell-session-all-modules "haskell-modules" "\
Get all modules -- installed or in the current project.
If DONTCREATE is non-nil don't create a new session.

\(fn SESSION &optional DONTCREATE)" nil nil)

(autoload 'haskell-session-project-modules "haskell-modules" "\
Get the modules of the current project.
If DONTCREATE is non-nil don't create a new session.

\(fn SESSION &optional DONTCREATE)" nil nil)

(register-definition-prefixes "haskell-modules" '("haskell-"))

;;;***

;;;### (autoloads nil "haskell-move-nested" "haskell-move-nested.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-move-nested.el

(autoload 'haskell-move-nested "haskell-move-nested" "\
Shift the nested off-side-rule block adjacent to point by COLS columns to the right.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" nil nil)

(autoload 'haskell-move-nested-right "haskell-move-nested" "\
Increase indentation of the following off-side-rule block adjacent to point.

Use a numeric prefix argument to indicate amount of indentation to apply.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" t nil)

(autoload 'haskell-move-nested-left "haskell-move-nested" "\
Decrease indentation of the following off-side-rule block adjacent to point.

Use a numeric prefix argument to indicate amount of indentation to apply.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" t nil)

(register-definition-prefixes "haskell-move-nested" '("haskell-"))

;;;***

;;;### (autoloads nil "haskell-navigate-imports" "haskell-navigate-imports.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-navigate-imports.el

(autoload 'haskell-navigate-imports "haskell-navigate-imports" "\
Cycle the Haskell import lines or return to point (with prefix arg).

\(fn &optional RETURN)" t nil)

(autoload 'haskell-navigate-imports-go "haskell-navigate-imports" "\
Go to the first line of a list of consecutive import lines. Cycles." t nil)

(autoload 'haskell-navigate-imports-return "haskell-navigate-imports" "\
Return to the non-import point we were at before going to the module list.
   If we were originally at an import list, we can just cycle through easily." t nil)

(register-definition-prefixes "haskell-navigate-imports" '("haskell-navigate-imports-"))

;;;***

;;;### (autoloads nil "haskell-presentation-mode" "haskell-presentation-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-presentation-mode.el

(register-definition-prefixes "haskell-presentation-mode" '("haskell-presentation-"))

;;;***

;;;### (autoloads nil "haskell-process" "haskell-process.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from haskell-process.el

(register-definition-prefixes "haskell-process" '("haskell-"))

;;;***

;;;### (autoloads nil "haskell-repl" "haskell-repl.el" (0 0 0 0))
;;; Generated autoloads from haskell-repl.el

(register-definition-prefixes "haskell-repl" '("haskell-interactive-"))

;;;***

;;;### (autoloads nil "haskell-sandbox" "haskell-sandbox.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from haskell-sandbox.el

(register-definition-prefixes "haskell-sandbox" '("haskell-sandbox-"))

;;;***

;;;### (autoloads nil "haskell-session" "haskell-session.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from haskell-session.el

(autoload 'haskell-session-maybe "haskell-session" "\
Maybe get the Haskell session, return nil if there isn't one." nil nil)

(autoload 'haskell-session-process "haskell-session" "\
Get the session process.

\(fn S)" nil nil)

(register-definition-prefixes "haskell-session" '("haskell-session"))

;;;***

;;;### (autoloads nil "haskell-sort-imports" "haskell-sort-imports.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-sort-imports.el

(autoload 'haskell-sort-imports "haskell-sort-imports" "\
Sort the import list at point. It sorts the current group
i.e. an import list separated by blank lines on either side.

If the region is active, it will restrict the imports to sort
within that region." t nil)

(register-definition-prefixes "haskell-sort-imports" '("haskell-sort-imports-"))

;;;***

;;;### (autoloads nil "haskell-string" "haskell-string.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from haskell-string.el

(register-definition-prefixes "haskell-string" '("haskell-"))

;;;***

;;;### (autoloads nil "haskell-svg" "haskell-svg.el" (0 0 0 0))
;;; Generated autoloads from haskell-svg.el

(register-definition-prefixes "haskell-svg" '("haskell-svg-"))

;;;***

;;;### (autoloads nil "haskell-unicode-input-method" "haskell-unicode-input-method.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-unicode-input-method.el

(autoload 'haskell-unicode-input-method-enable "haskell-unicode-input-method" "\
Set input method `haskell-unicode'." t nil)

(define-obsolete-function-alias 'turn-on-haskell-unicode-input-method 'haskell-unicode-input-method-enable "2020-04")

;;;***

;;;### (autoloads nil "haskell-utils" "haskell-utils.el" (0 0 0 0))
;;; Generated autoloads from haskell-utils.el

(register-definition-prefixes "haskell-utils" '("haskell-"))

;;;***

;;;### (autoloads nil "highlight-uses-mode" "highlight-uses-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from highlight-uses-mode.el

(autoload 'highlight-uses-mode "highlight-uses-mode" "\
Minor mode for highlighting and jumping between uses.

If called interactively, toggle `Highlight-Uses mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "highlight-uses-mode" '("highlight-uses-mode-"))

;;;***

;;;### (autoloads nil "inf-haskell" "inf-haskell.el" (0 0 0 0))
;;; Generated autoloads from inf-haskell.el

(let ((loads (get 'inferior-haskell 'custom-loads))) (if (member '"inf-haskell" loads) nil (put 'inferior-haskell 'custom-loads (cons '"inf-haskell" loads))))

(autoload 'run-haskell "inf-haskell" "\
Show the inferior-haskell buffer.  Start the process if needed." t nil)

(register-definition-prefixes "inf-haskell" '("haskell-" "inf"))

;;;***

;;;### (autoloads nil "w3m-haddock" "w3m-haddock.el" (0 0 0 0))
;;; Generated autoloads from w3m-haddock.el

(defface w3m-haddock-heading-face '((((class color)) :inherit highlight)) "\
Face for quarantines." :group 'haskell)

(register-definition-prefixes "w3m-haddock" '("haskell-w3m-" "w3m-haddock-"))

;;;***

;;;### (autoloads nil nil ("haskell-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; haskell-mode-autoloads.el ends here
;;; highlight-indentation-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "highlight-indentation" "highlight-indentation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from highlight-indentation.el

(autoload 'highlight-indentation-mode "highlight-indentation" "\
Highlight indentation minor mode highlights indentation based on spaces

If called interactively, enable Highlight-Indentation mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'highlight-indentation-set-offset "highlight-indentation" "\
Set indentation offset locally in buffer, will prevent
highlight-indentation from trying to guess indentation offset
from major mode

\(fn OFFSET)" t nil)

(autoload 'highlight-indentation-current-column-mode "highlight-indentation" "\
Highlight Indentation minor mode displays a vertical bar
corresponding to the indentation of the current line

If called interactively, enable
Highlight-Indentation-Current-Column mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "highlight-indentation" '("highlight-indentation-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; highlight-indentation-autoloads.el ends here
;;; hl-todo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hl-todo" "hl-todo.el" (0 0 0 0))
;;; Generated autoloads from hl-todo.el

(autoload 'hl-todo-mode "hl-todo" "\
Highlight TODO and similar keywords in comments and strings.

If called interactively, enable Hl-Todo mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-hl-todo-mode 'globalized-minor-mode t)

(defvar global-hl-todo-mode nil "\
Non-nil if Global Hl-Todo mode is enabled.
See the `global-hl-todo-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-hl-todo-mode'.")

(custom-autoload 'global-hl-todo-mode "hl-todo" nil)

(autoload 'global-hl-todo-mode "hl-todo" "\
Toggle Hl-Todo mode in all buffers.
With prefix ARG, enable Global Hl-Todo mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Hl-Todo mode is enabled in all buffers where
`hl-todo--turn-on-mode-if-desired' would do it.
See `hl-todo-mode' for more information on Hl-Todo mode.

\(fn &optional ARG)" t nil)

(autoload 'hl-todo-next "hl-todo" "\
Jump to the next TODO or similar keyword.
The prefix argument ARG specifies how many keywords to move.
A negative argument means move backward that many keywords.

\(fn ARG)" t nil)

(autoload 'hl-todo-previous "hl-todo" "\
Jump to the previous TODO or similar keyword.
The prefix argument ARG specifies how many keywords to move.
A negative argument means move forward that many keywords.

\(fn ARG)" t nil)

(autoload 'hl-todo-occur "hl-todo" "\
Use `occur' to find all TODO or similar keywords.
This actually finds a superset of the highlighted keywords,
because it uses a regexp instead of a more sophisticated
matcher.  It also finds occurrences that are not within a
string or comment." t nil)

(autoload 'hl-todo-insert "hl-todo" "\
Insert TODO or similar keyword.
If point is not inside a string or comment, then insert a new
comment.  If point is at the end of the line, then insert the
comment there, otherwise insert it as a new line before the
current line.

\(fn KEYWORD)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hl-todo" '("hl-todo-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hl-todo-autoloads.el ends here
;;; ht-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ht" "ht.el" (0 0 0 0))
;;; Generated autoloads from ht.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ht" 'nil))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ht-autoloads.el ends here
;;; htmlize-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "htmlize" "htmlize.el" (0 0 0 0))
;;; Generated autoloads from htmlize.el

(autoload 'htmlize-buffer "htmlize" "\
Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses.

\(fn &optional BUFFER)" t nil)

(autoload 'htmlize-region "htmlize" "\
Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details.

\(fn BEG END)" t nil)

(autoload 'htmlize-file "htmlize" "\
Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name.

\(fn FILE &optional TARGET)" t nil)

(autoload 'htmlize-many-files "htmlize" "\
Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file.

\(fn FILES &optional TARGET-DIRECTORY)" t nil)

(autoload 'htmlize-many-files-dired "htmlize" "\
HTMLize dired-marked files.

\(fn ARG &optional TARGET-DIRECTORY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "htmlize" '("htmlize-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; htmlize-autoloads.el ends here
;;; hydra-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hydra" "hydra.el" (0 0 0 0))
;;; Generated autoloads from hydra.el

(autoload 'defhydra "hydra" "\
Create a Hydra - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest BODY-PLIST)

DOCSTRING will be displayed in the echo area to identify the
Hydra.  When DOCSTRING starts with a newline, special Ruby-style
substitution will be performed by `hydra--format'.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY (both are strings passed to `kbd'), and will set
the transient map so that all following heads can be called
though KEY only.  BODY-KEY can be an empty string.

CMD is a callable expression: either an interactive function
name, or an interactive lambda, or a single sexp (it will be
wrapped in an interactive lambda).

HINT is a short string that identifies its head.  It will be
printed beside KEY in the echo erea if `hydra-is-helpful' is not
nil.  If you don't even want the KEY to be printed, set HINT
explicitly to nil.

The heads inherit their PLIST from BODY-PLIST and are allowed to
override some keys.  The keys recognized are :exit, :bind, and :column.
:exit can be:

- nil (default): this head will continue the Hydra state.
- t: this head will stop the Hydra state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head.

:column is a string that sets the column for all subsequent heads.

It is possible to omit both BODY-MAP and BODY-KEY if you don't
want to bind anything.  In that case, typically you will bind the
generated NAME/body command.  This command is also the return
result of `defhydra'.

\(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t)

(function-put 'defhydra 'lisp-indent-function 'defun)

(function-put 'defhydra 'doc-string-elt '3)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra" '("defhydra" "hydra-")))

;;;***

;;;### (autoloads nil "hydra-examples" "hydra-examples.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from hydra-examples.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra-examples" '("hydra-" "org-agenda-cts" "whitespace-mode")))

;;;***

;;;### (autoloads nil "hydra-ox" "hydra-ox.el" (0 0 0 0))
;;; Generated autoloads from hydra-ox.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra-ox" '("hydra-ox")))

;;;***

;;;### (autoloads nil nil ("hydra-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hydra-autoloads.el ends here
;;; icomplete-vertical-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "icomplete-vertical" "icomplete-vertical.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from icomplete-vertical.el

(defvar icomplete-vertical-mode nil "\
Non-nil if Icomplete-Vertical mode is enabled.
See the `icomplete-vertical-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `icomplete-vertical-mode'.")

(custom-autoload 'icomplete-vertical-mode "icomplete-vertical" nil)

(autoload 'icomplete-vertical-mode "icomplete-vertical" "\
Display icomplete candidates vertically.

If called interactively, enable Icomplete-Vertical mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'icomplete-vertical-toggle "icomplete-vertical" "\
Toggle Icomplete Vertical mode without echo area message." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "icomplete-vertical" '("icomplete-vertical-")))

;;;***

;;;### (autoloads nil nil ("icomplete-vertical-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; icomplete-vertical-autoloads.el ends here
;;; iedit-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "iedit" "iedit.el" (0 0 0 0))
;;; Generated autoloads from iedit.el

(autoload 'iedit-mode "iedit" "\
Toggle Iedit mode.
This command behaves differently, depending on the mark, point,
prefix argument and variable `iedit-transient-mark-sensitive'.

If Iedit mode is off, turn Iedit mode on.

When Iedit mode is turned on, all the occurrences of the current
region in the buffer (possibly narrowed) or a region are
highlighted.  If one occurrence is modified, the change are
propagated to all other occurrences simultaneously.

If region is not active, `iedit-default-occurrence' is called to
get an occurrence candidate, according to the thing at point.  It
might be url, email address, markup tag or current symbol(or
word).

In the above two situations, with digit prefix argument 0, only
occurrences in current function are matched.  This is good for
renaming refactoring in programming.

You can also switch to Iedit mode from isearch mode directly. The
current search string is used as occurrence.  All occurrences of
the current search string are highlighted.

With an universal prefix argument, the occurrence when Iedit mode
is turned off last time in current buffer is used as occurrence.
This is intended to recover last Iedit mode which is turned off.
If region active, Iedit mode is limited within the current
region.

With repeated universal prefix argument, the occurrence when
Iedit mode is turned off last time (might be in other buffer) is
used as occurrence.  If region active, Iedit mode is limited
within the current region.

With digital prefix argument 1, Iedit mode is limited on the
current symbol or the active region, which means just one
instance is highlighted.  This behavior serves as a start point
of incremental selection work flow.

If Iedit mode is on and region is active, Iedit mode is
restricted in the region, e.g. the occurrences outside of the
region is excluded.

If Iedit mode is on and region is active, with an universal
prefix argument, Iedit mode is restricted outside of the region,
e.g. the occurrences in the region is excluded.

Turn off Iedit mode in other situations.

Commands:
\\{iedit-mode-keymap}
Keymap used within overlays:
\\{iedit-mode-occurrence-keymap}

\(fn &optional ARG)" t nil)

(autoload 'iedit-mode-toggle-on-function "iedit" "\
Toggle Iedit mode on current function." t nil)

(register-definition-prefixes "iedit" '("iedit-"))

;;;***

;;;### (autoloads nil "iedit-lib" "iedit-lib.el" (0 0 0 0))
;;; Generated autoloads from iedit-lib.el

(register-definition-prefixes "iedit-lib" '("iedit-"))

;;;***

;;;### (autoloads nil "iedit-rect" "iedit-rect.el" (0 0 0 0))
;;; Generated autoloads from iedit-rect.el

(autoload 'iedit-rectangle-mode "iedit-rect" "\
Toggle Iedit-rect mode.

When Iedit-rect mode is on, a rectangle is started with visible
rectangle highlighting.  Rectangle editing support is based on
Iedit mechanism.

Commands:
\\{iedit-rect-keymap}

\(fn &optional BEG END)" t nil)

(register-definition-prefixes "iedit-rect" '("iedit-rect"))

;;;***

;;;### (autoloads nil nil ("iedit-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; iedit-autoloads.el ends here
;;; ivy-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "colir" "colir.el" (0 0 0 0))
;;; Generated autoloads from colir.el

(register-definition-prefixes "colir" '("colir-"))

;;;***

;;;### (autoloads nil "ivy" "ivy.el" (0 0 0 0))
;;; Generated autoloads from ivy.el

(autoload 'ivy-resume "ivy" "\
Resume the last completion session, or SESSION if non-nil.
With a prefix arg, try to restore a recorded completion session,
if one exists.

\(fn &optional SESSION)" t nil)

(autoload 'ivy-read "ivy" "\
Read a string in the minibuffer, with completion.

PROMPT is a string, normally ending in a colon and a space.
`ivy-count-format' is prepended to PROMPT during completion.

COLLECTION is either a list of strings, a function, an alist, or
a hash table, supplied for `minibuffer-completion-table'.

PREDICATE is applied to filter out the COLLECTION immediately.
This argument is for compatibility with `completing-read'.

When REQUIRE-MATCH is non-nil, only members of COLLECTION can be
selected.

If INITIAL-INPUT is non-nil, then insert that input in the
minibuffer initially.

HISTORY is a name of a variable to hold the completion session
history.

KEYMAP is composed with `ivy-minibuffer-map'.

PRESELECT, when non-nil, determines which one of the candidates
matching INITIAL-INPUT to select initially.  An integer stands
for the position of the desired candidate in the collection,
counting from zero.  Otherwise, use the first occurrence of
PRESELECT in the collection.  Comparison is first done with
`equal'.  If that fails, and when applicable, match PRESELECT as
a regular expression.

DEF is for compatibility with `completing-read'.

UPDATE-FN is called each time the candidate list is re-displayed.

When SORT is non-nil, `ivy-sort-functions-alist' determines how
to sort candidates before displaying them.

ACTION is a function to call after selecting a candidate.
It takes one argument, the selected candidate. If COLLECTION is
an alist, the argument is a cons cell, otherwise it's a string.

MULTI-ACTION, when non-nil, is called instead of ACTION when
there are marked candidates. It takes the list of candidates as
its only argument. When it's nil, ACTION is called on each marked
candidate.

UNWIND is a function of no arguments to call before exiting.

RE-BUILDER is a function transforming input text into a regex
pattern.

MATCHER is a function which can override how candidates are
filtered based on user input.  It takes a regex pattern and a
list of candidates, and returns the list of matching candidates.

DYNAMIC-COLLECTION is a boolean specifying whether the list of
candidates is updated after each input by calling COLLECTION.

EXTRA-PROPS is a plist that can be used to store
collection-specific session-specific data.

CALLER is a symbol to uniquely identify the caller to `ivy-read'.
It is used, along with COLLECTION, to determine which
customizations apply to the current completion session.

\(fn PROMPT COLLECTION &key PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY PRESELECT DEF KEYMAP UPDATE-FN SORT ACTION MULTI-ACTION UNWIND RE-BUILDER MATCHER DYNAMIC-COLLECTION EXTRA-PROPS CALLER)" nil nil)

(autoload 'ivy-completing-read "ivy" "\
Read a string in the minibuffer, with completion.

This interface conforms to `completing-read' and can be used for
`completing-read-function'.

PROMPT is a string that normally ends in a colon and a space.
COLLECTION is either a list of strings, an alist, an obarray, or a hash table.
PREDICATE limits completion to a subset of COLLECTION.
REQUIRE-MATCH is a boolean value or a symbol.  See `completing-read'.
INITIAL-INPUT is a string inserted into the minibuffer initially.
HISTORY is a list of previously selected inputs.
DEF is the default value.
INHERIT-INPUT-METHOD is currently ignored.

\(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY DEF INHERIT-INPUT-METHOD)" nil nil)

(defvar ivy-mode nil "\
Non-nil if Ivy mode is enabled.
See the `ivy-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-mode'.")

(custom-autoload 'ivy-mode "ivy" nil)

(autoload 'ivy-mode "ivy" "\
Toggle Ivy mode on or off.
Turn Ivy mode on if ARG is positive, off otherwise.
Turning on Ivy mode sets `completing-read-function' to
`ivy-completing-read'.

Global bindings:
\\{ivy-mode-map}

Minibuffer bindings:
\\{ivy-minibuffer-map}

\(fn &optional ARG)" t nil)

(autoload 'ivy-switch-buffer "ivy" "\
Switch to another buffer." t nil)

(autoload 'ivy-switch-view "ivy" "\
Switch to one of the window views stored by `ivy-push-view'." t nil)

(autoload 'ivy-switch-buffer-other-window "ivy" "\
Switch to another buffer in another window." t nil)

(register-definition-prefixes "ivy" '("ivy-" "with-ivy-window"))

;;;***

;;;### (autoloads nil "ivy-overlay" "ivy-overlay.el" (0 0 0 0))
;;; Generated autoloads from ivy-overlay.el

(register-definition-prefixes "ivy-overlay" '("ivy-"))

;;;***

;;;### (autoloads nil nil ("elpa.el" "ivy-faces.el" "ivy-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-autoloads.el ends here
;;; ivy-posframe-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-posframe" "ivy-posframe.el" (0 0 0 0))
;;; Generated autoloads from ivy-posframe.el

(defvar ivy-posframe-mode nil "\
Non-nil if Ivy-Posframe mode is enabled.
See the `ivy-posframe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-posframe-mode'.")

(custom-autoload 'ivy-posframe-mode "ivy-posframe" nil)

(autoload 'ivy-posframe-mode "ivy-posframe" "\
Display ivy via posframe.

If called interactively, toggle `Ivy-Posframe mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "ivy-posframe" '("ivy-posframe-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-posframe-autoloads.el ends here
;;; jade-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "jade-mode" "jade-mode.el" (0 0 0 0))
;;; Generated autoloads from jade-mode.el

(autoload 'jade-mode "jade-mode" "\
Major mode for editing jade node.js templates

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))

(add-to-list 'auto-mode-alist '("\\.pug\\'" . jade-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jade-mode" '("jade-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jade-mode-autoloads.el ends here
;;; js2-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "js2-imenu-extras" "js2-imenu-extras.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from js2-imenu-extras.el

(autoload 'js2-imenu-extras-setup "js2-imenu-extras" nil nil nil)

(autoload 'js2-imenu-extras-mode "js2-imenu-extras" "\
Toggle Imenu support for frameworks and structural patterns.

If called interactively, enable Js2-Imenu-Extras mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2-imenu-extras" '("js2-imenu-")))

;;;***

;;;### (autoloads nil "js2-mode" "js2-mode.el" (0 0 0 0))
;;; Generated autoloads from js2-mode.el

(autoload 'js2-highlight-unused-variables-mode "js2-mode" "\
Toggle highlight of unused variables.

If called interactively, enable Js2-Highlight-Unused-Variables
mode if ARG is positive, and disable it if ARG is zero or
negative.  If called from Lisp, also enable the mode if ARG is
omitted or nil, and toggle it if ARG is `toggle'; disable the
mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'js2-minor-mode "js2-mode" "\
Minor mode for running js2 as a background linter.
This allows you to use a different major mode for JavaScript editing,
such as `js-mode', while retaining the asynchronous error/warning
highlighting features of `js2-mode'.

If called interactively, enable Js2 minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'js2-mode "js2-mode" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

(autoload 'js2-jsx-mode "js2-mode" "\
Major mode for editing JSX code in Emacs 26 and earlier.

To edit JSX code in Emacs 27, use `js-mode' as your major mode
with `js2-minor-mode' enabled.

To customize the indentation for this mode, set the SGML offset
variables (`sgml-basic-offset' et al) locally, like so:

  (defun set-jsx-indentation ()
    (setq-local sgml-basic-offset js2-basic-offset))
  (add-hook \\='js2-jsx-mode-hook #\\='set-jsx-indentation)

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2-mode" '("js2-")))

;;;***

;;;### (autoloads nil "js2-old-indent" "js2-old-indent.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from js2-old-indent.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2-old-indent" '("js2-")))

;;;***

;;;### (autoloads nil nil ("js2-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; js2-mode-autoloads.el ends here
;;; kana-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "kana" "kana.el" (0 0 0 0))
;;; Generated autoloads from kana.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kana" '("kana")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; kana-autoloads.el ends here
;;; lcr-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lcr" "lcr.el" (0 0 0 0))
;;; Generated autoloads from lcr.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lcr" '("lcr-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lcr-autoloads.el ends here
;;; liberime-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "liberime" "liberime.el" (0 0 0 0))
;;; Generated autoloads from liberime.el

(autoload 'liberime-open-user-data-dir "liberime" "\
Open user data dir with external app." t nil)

(autoload 'liberime-open-shared-data-dir "liberime" "\
Open shared data dir with external app." t nil)

(autoload 'liberime-open-package-directory "liberime" "\
Open liberime library directory with external app." t nil)

(autoload 'liberime-open-package-readme "liberime" "\
Open liberime library README.org." t nil)

(autoload 'liberime-build "liberime" "\
Build liberime-core module." t nil)

(autoload 'liberime-load "liberime" "\
Load liberime-core module." t nil)

(autoload 'liberime-deploy "liberime" "\
Deploy liberime to affect config file change." t nil)

(autoload 'liberime-set-page-size "liberime" "\
Set rime page-size to PAGE-SIZE or by default 10.
you also need to call `liberime-deploy' to make it take affect
you only need to do this once.

\(fn PAGE-SIZE)" t nil)

(autoload 'liberime-select-schema-interactive "liberime" "\
Select a rime schema interactive." t nil)

(autoload 'liberime-sync "liberime" "\
Sync rime user data.
User should specify sync_dir in installation.yaml file of
`liberime-user-data-dir' directory." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "liberime" '("liberime-")))

;;;***

;;;### (autoloads nil nil ("liberime-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; liberime-autoloads.el ends here
;;; load-bash-alias-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "load-bash-alias" "load-bash-alias.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from load-bash-alias.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "load-bash-alias" '("load-bash-alias-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; load-bash-alias-autoloads.el ends here
;;; lsp-java-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dap-java" "dap-java.el" (0 0 0 0))
;;; Generated autoloads from dap-java.el
(with-eval-after-load 'lsp-java (require 'dap-java))

(register-definition-prefixes "dap-java" '("dap-java-"))

;;;***

;;;### (autoloads nil "lsp-java" "lsp-java.el" (0 0 0 0))
;;; Generated autoloads from lsp-java.el

(autoload 'lsp-java-lens-mode "lsp-java" "\
Toggle run/debug overlays.

If called interactively, toggle `Lsp-Java-Lens mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "lsp-java" '("lsp-java-"))

;;;***

;;;### (autoloads nil "lsp-java-boot" "lsp-java-boot.el" (0 0 0 0))
;;; Generated autoloads from lsp-java-boot.el

(autoload 'lsp-java-boot-lens-mode "lsp-java-boot" "\
Toggle code-lens overlays.

If called interactively, toggle `Lsp-Java-Boot-Lens mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "lsp-java-boot" '("lsp-java-boot-"))

;;;***

;;;### (autoloads nil "lsp-jt" "lsp-jt.el" (0 0 0 0))
;;; Generated autoloads from lsp-jt.el

(autoload 'lsp-jt-lens-mode "lsp-jt" "\
Toggle code-lens overlays.

If called interactively, toggle `Lsp-Jt-Lens mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'lsp-jt-browser "lsp-jt" nil t nil)

(register-definition-prefixes "lsp-jt" '("lsp-jt-"))

;;;***

;;;### (autoloads nil nil ("lsp-java-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-java-autoloads.el ends here
;;; lsp-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-actionscript" "lsp-actionscript.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from lsp-actionscript.el

(register-definition-prefixes "lsp-actionscript" '("lsp-actionscript-"))

;;;***

;;;### (autoloads nil "lsp-ada" "lsp-ada.el" (0 0 0 0))
;;; Generated autoloads from lsp-ada.el

(register-definition-prefixes "lsp-ada" '("lsp-ada-"))

;;;***

;;;### (autoloads nil "lsp-angular" "lsp-angular.el" (0 0 0 0))
;;; Generated autoloads from lsp-angular.el

(register-definition-prefixes "lsp-angular" '("lsp-client"))

;;;***

;;;### (autoloads nil "lsp-bash" "lsp-bash.el" (0 0 0 0))
;;; Generated autoloads from lsp-bash.el

(register-definition-prefixes "lsp-bash" '("lsp-bash-"))

;;;***

;;;### (autoloads nil "lsp-clangd" "lsp-clangd.el" (0 0 0 0))
;;; Generated autoloads from lsp-clangd.el

(autoload 'lsp-cpp-flycheck-clang-tidy-error-explainer "lsp-clangd" "\
Explain a clang-tidy ERROR by scraping documentation from llvm.org.

\(fn ERROR)" nil nil)

(register-definition-prefixes "lsp-clangd" '("lsp-c"))

;;;***

;;;### (autoloads nil "lsp-clojure" "lsp-clojure.el" (0 0 0 0))
;;; Generated autoloads from lsp-clojure.el

(register-definition-prefixes "lsp-clojure" '("lsp-clojure-"))

;;;***

;;;### (autoloads nil "lsp-completion" "lsp-completion.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lsp-completion.el

(define-obsolete-variable-alias 'lsp-prefer-capf 'lsp-completion-provider "lsp-mode 7.0.1")

(define-obsolete-variable-alias 'lsp-enable-completion-at-point 'lsp-completion-enable "lsp-mode 7.0.1")

(autoload 'lsp-completion-at-point "lsp-completion" "\
Get lsp completions." nil nil)

(autoload 'lsp-completion--enable "lsp-completion" "\
Enable LSP completion support." nil nil)

(autoload 'lsp-completion-mode "lsp-completion" "\
Toggle LSP completion support.

If called interactively, toggle `Lsp-Completion mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(add-hook 'lsp-configure-hook (lambda nil (when (and lsp-auto-configure lsp-completion-enable) (lsp-completion--enable))))

(register-definition-prefixes "lsp-completion" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-crystal" "lsp-crystal.el" (0 0 0 0))
;;; Generated autoloads from lsp-crystal.el

(register-definition-prefixes "lsp-crystal" '("lsp-clients-crystal-executable"))

;;;***

;;;### (autoloads nil "lsp-csharp" "lsp-csharp.el" (0 0 0 0))
;;; Generated autoloads from lsp-csharp.el

(register-definition-prefixes "lsp-csharp" '("lsp-csharp-"))

;;;***

;;;### (autoloads nil "lsp-css" "lsp-css.el" (0 0 0 0))
;;; Generated autoloads from lsp-css.el

(register-definition-prefixes "lsp-css" '("lsp-css-"))

;;;***

;;;### (autoloads nil "lsp-diagnostics" "lsp-diagnostics.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from lsp-diagnostics.el

(define-obsolete-variable-alias 'lsp-diagnostic-package 'lsp-diagnostics-provider "lsp-mode 7.0.1")

(define-obsolete-variable-alias 'lsp-flycheck-default-level 'lsp-diagnostics-flycheck-default-level "lsp-mode 7.0.1")

(autoload 'lsp-diagnostics--enable "lsp-diagnostics" "\
Enable LSP checker support." nil nil)

(autoload 'lsp-diagnostics-mode "lsp-diagnostics" "\
Toggle LSP diagnostics integration.

If called interactively, toggle `Lsp-Diagnostics mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(add-hook 'lsp-configure-hook (lambda nil (when lsp-auto-configure (lsp-diagnostics--enable))))

(register-definition-prefixes "lsp-diagnostics" '("lsp-diagnostics-"))

;;;***

;;;### (autoloads nil "lsp-dired" "lsp-dired.el" (0 0 0 0))
;;; Generated autoloads from lsp-dired.el

(defvar lsp-dired-mode nil "\
Non-nil if Lsp-Dired mode is enabled.
See the `lsp-dired-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lsp-dired-mode'.")

(custom-autoload 'lsp-dired-mode "lsp-dired" nil)

(autoload 'lsp-dired-mode "lsp-dired" "\
Display `lsp-mode' icons for each file in a dired buffer.

If called interactively, toggle `Lsp-Dired mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "lsp-dired" '("lsp-dired-"))

;;;***

;;;### (autoloads nil "lsp-dockerfile" "lsp-dockerfile.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lsp-dockerfile.el

(register-definition-prefixes "lsp-dockerfile" '("lsp-dockerfile-language-server-command"))

;;;***

;;;### (autoloads nil "lsp-elixir" "lsp-elixir.el" (0 0 0 0))
;;; Generated autoloads from lsp-elixir.el

(register-definition-prefixes "lsp-elixir" '("lsp-elixir-"))

;;;***

;;;### (autoloads nil "lsp-elm" "lsp-elm.el" (0 0 0 0))
;;; Generated autoloads from lsp-elm.el

(register-definition-prefixes "lsp-elm" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-erlang" "lsp-erlang.el" (0 0 0 0))
;;; Generated autoloads from lsp-erlang.el

(register-definition-prefixes "lsp-erlang" '("lsp-erlang-server-"))

;;;***

;;;### (autoloads nil "lsp-eslint" "lsp-eslint.el" (0 0 0 0))
;;; Generated autoloads from lsp-eslint.el

(register-definition-prefixes "lsp-eslint" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-fortran" "lsp-fortran.el" (0 0 0 0))
;;; Generated autoloads from lsp-fortran.el

(register-definition-prefixes "lsp-fortran" '("lsp-clients-"))

;;;***

;;;### (autoloads nil "lsp-fsharp" "lsp-fsharp.el" (0 0 0 0))
;;; Generated autoloads from lsp-fsharp.el

(autoload 'lsp-fsharp--workspace-load "lsp-fsharp" "\
Load all of the provided PROJECTS.

\(fn PROJECTS)" nil nil)

(register-definition-prefixes "lsp-fsharp" '("lsp-fsharp-"))

;;;***

;;;### (autoloads nil "lsp-gdscript" "lsp-gdscript.el" (0 0 0 0))
;;; Generated autoloads from lsp-gdscript.el

(register-definition-prefixes "lsp-gdscript" '("lsp-gdscript-"))

;;;***

;;;### (autoloads nil "lsp-go" "lsp-go.el" (0 0 0 0))
;;; Generated autoloads from lsp-go.el

(register-definition-prefixes "lsp-go" '("lsp-go-"))

;;;***

;;;### (autoloads nil "lsp-groovy" "lsp-groovy.el" (0 0 0 0))
;;; Generated autoloads from lsp-groovy.el

(register-definition-prefixes "lsp-groovy" '("lsp-groovy-"))

;;;***

;;;### (autoloads nil "lsp-hack" "lsp-hack.el" (0 0 0 0))
;;; Generated autoloads from lsp-hack.el

(register-definition-prefixes "lsp-hack" '("lsp-clients-hack-command"))

;;;***

;;;### (autoloads nil "lsp-haxe" "lsp-haxe.el" (0 0 0 0))
;;; Generated autoloads from lsp-haxe.el

(register-definition-prefixes "lsp-haxe" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-headerline" "lsp-headerline.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lsp-headerline.el

(autoload 'lsp-headerline-breadcrumb-mode "lsp-headerline" "\
Toggle breadcrumb on headerline.

If called interactively, toggle `Lsp-Headerline-Breadcrumb mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'lsp-breadcrumb-go-to-symbol "lsp-headerline" "\
Go to the symbol on breadcrumb at SYMBOL-POSITION.

\(fn SYMBOL-POSITION)" t nil)

(autoload 'lsp-breadcrumb-narrow-to-symbol "lsp-headerline" "\
Narrow to the symbol range on breadcrumb at SYMBOL-POSITION.

\(fn SYMBOL-POSITION)" t nil)

(register-definition-prefixes "lsp-headerline" '("lsp-headerline-"))

;;;***

;;;### (autoloads nil "lsp-html" "lsp-html.el" (0 0 0 0))
;;; Generated autoloads from lsp-html.el

(register-definition-prefixes "lsp-html" '("lsp-html-"))

;;;***

;;;### (autoloads nil "lsp-icons" "lsp-icons.el" (0 0 0 0))
;;; Generated autoloads from lsp-icons.el

(register-definition-prefixes "lsp-icons" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-ido" "lsp-ido.el" (0 0 0 0))
;;; Generated autoloads from lsp-ido.el

(autoload 'lsp-ido-workspace-symbol "lsp-ido" "\
`ido' for lsp workspace/symbol.
When called with prefix ARG the default selection will be symbol at point.

\(fn ARG)" t nil)

(register-definition-prefixes "lsp-ido" '("lsp-ido-"))

;;;***

;;;### (autoloads nil "lsp-iedit" "lsp-iedit.el" (0 0 0 0))
;;; Generated autoloads from lsp-iedit.el

(autoload 'lsp-iedit-highlights "lsp-iedit" "\
Start an `iedit' operation on the documentHighlights at point.
This can be used as a primitive `lsp-rename' replacement if the
language server doesn't support renaming.

See also `lsp-enable-symbol-highlighting'." t nil)

(autoload 'lsp-evil-multiedit-highlights "lsp-iedit" "\
Start an `evil-multiedit' operation on the documentHighlights at point.
This can be used as a primitive `lsp-rename' replacement if the
language server doesn't support renaming.

See also `lsp-enable-symbol-highlighting'." t nil)

(register-definition-prefixes "lsp-iedit" '("lsp-iedit--on-ranges"))

;;;***

;;;### (autoloads nil "lsp-javascript" "lsp-javascript.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lsp-javascript.el

(register-definition-prefixes "lsp-javascript" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-json" "lsp-json.el" (0 0 0 0))
;;; Generated autoloads from lsp-json.el

(register-definition-prefixes "lsp-json" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-kotlin" "lsp-kotlin.el" (0 0 0 0))
;;; Generated autoloads from lsp-kotlin.el

(register-definition-prefixes "lsp-kotlin" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-lens" "lsp-lens.el" (0 0 0 0))
;;; Generated autoloads from lsp-lens.el

(autoload 'lsp-lens--enable "lsp-lens" "\
Enable lens mode." nil nil)

(autoload 'lsp-lens-show "lsp-lens" "\
Display lenses in the buffer." t nil)

(autoload 'lsp-lens-hide "lsp-lens" "\
Delete all lenses." t nil)

(autoload 'lsp-lens-mode "lsp-lens" "\
Toggle code-lens overlays.

If called interactively, toggle `Lsp-Lens mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'lsp-avy-lens "lsp-lens" "\
Click lsp lens using `avy' package." t nil)

(register-definition-prefixes "lsp-lens" '("lsp-lens-"))

;;;***

;;;### (autoloads nil "lsp-lua" "lsp-lua.el" (0 0 0 0))
;;; Generated autoloads from lsp-lua.el

(register-definition-prefixes "lsp-lua" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-mode" "lsp-mode.el" (0 0 0 0))
;;; Generated autoloads from lsp-mode.el
(put 'lsp-enable-file-watchers 'safe-local-variable #'booleanp)
(put 'lsp-file-watch-threshold 'safe-local-variable (lambda (i) (or (numberp i) (not i))))

(autoload 'lsp "lsp-mode" "\
Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be opened in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start.

\(fn &optional ARG)" t nil)

(autoload 'lsp-deferred "lsp-mode" "\
Entry point that defers server startup until buffer is visible.
`lsp-deferred' will wait until the buffer is visible before invoking `lsp'.
This avoids overloading the server with many files when starting Emacs." nil nil)

(register-definition-prefixes "lsp-mode" '("lsp-" "make-lsp-client" "when-lsp-workspace" "with-lsp-workspace"))

;;;***

;;;### (autoloads nil "lsp-modeline" "lsp-modeline.el" (0 0 0 0))
;;; Generated autoloads from lsp-modeline.el

(define-obsolete-variable-alias 'lsp-diagnostics-modeline-scope 'lsp-modeline-diagnostics-scope "lsp-mode 7.0.1")

(autoload 'lsp-modeline-code-actions-mode "lsp-modeline" "\
Toggle code actions on modeline.

If called interactively, toggle `Lsp-Modeline-Code-Actions mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'lsp-diagnostics-modeline-mode 'lsp-modeline-diagnostics-mode "lsp-mode 7.0.1")

(autoload 'lsp-modeline-diagnostics-mode "lsp-modeline" "\
Toggle diagnostics modeline.

If called interactively, toggle `Lsp-Modeline-Diagnostics mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'lsp-modeline-workspace-status-mode "lsp-modeline" "\
Toggle workspace status on modeline.

If called interactively, toggle `Lsp-Modeline-Workspace-Status
mode'.  If the prefix argument is positive, enable the mode, and
if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "lsp-modeline" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-nix" "lsp-nix.el" (0 0 0 0))
;;; Generated autoloads from lsp-nix.el

(register-definition-prefixes "lsp-nix" '("lsp-nix-server-path"))

;;;***

;;;### (autoloads nil "lsp-ocaml" "lsp-ocaml.el" (0 0 0 0))
;;; Generated autoloads from lsp-ocaml.el

(register-definition-prefixes "lsp-ocaml" '("lsp-ocaml-l"))

;;;***

;;;### (autoloads nil "lsp-perl" "lsp-perl.el" (0 0 0 0))
;;; Generated autoloads from lsp-perl.el

(register-definition-prefixes "lsp-perl" '("lsp-perl-"))

;;;***

;;;### (autoloads nil "lsp-php" "lsp-php.el" (0 0 0 0))
;;; Generated autoloads from lsp-php.el

(register-definition-prefixes "lsp-php" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-prolog" "lsp-prolog.el" (0 0 0 0))
;;; Generated autoloads from lsp-prolog.el

(register-definition-prefixes "lsp-prolog" '("lsp-prolog-server-command"))

;;;***

;;;### (autoloads nil "lsp-protocol" "lsp-protocol.el" (0 0 0 0))
;;; Generated autoloads from lsp-protocol.el

(register-definition-prefixes "lsp-protocol" '("dash-expand:&RangeToPoint" "lsp"))

;;;***

;;;### (autoloads nil "lsp-purescript" "lsp-purescript.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lsp-purescript.el

(register-definition-prefixes "lsp-purescript" '("lsp-purescript-"))

;;;***

;;;### (autoloads nil "lsp-pwsh" "lsp-pwsh.el" (0 0 0 0))
;;; Generated autoloads from lsp-pwsh.el

(register-definition-prefixes "lsp-pwsh" '("lsp-pwsh-"))

;;;***

;;;### (autoloads nil "lsp-pyls" "lsp-pyls.el" (0 0 0 0))
;;; Generated autoloads from lsp-pyls.el

(register-definition-prefixes "lsp-pyls" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-r" "lsp-r.el" (0 0 0 0))
;;; Generated autoloads from lsp-r.el

(register-definition-prefixes "lsp-r" '("lsp-clients-r-server-command"))

;;;***

;;;### (autoloads nil "lsp-racket" "lsp-racket.el" (0 0 0 0))
;;; Generated autoloads from lsp-racket.el

(register-definition-prefixes "lsp-racket" '("lsp-racket-lang"))

;;;***

;;;### (autoloads nil "lsp-rf" "lsp-rf.el" (0 0 0 0))
;;; Generated autoloads from lsp-rf.el

(register-definition-prefixes "lsp-rf" '("expand-start-command" "lsp-rf-language-server-" "parse-rf-language-server-"))

;;;***

;;;### (autoloads nil "lsp-rust" "lsp-rust.el" (0 0 0 0))
;;; Generated autoloads from lsp-rust.el

(register-definition-prefixes "lsp-rust" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-semantic-tokens" "lsp-semantic-tokens.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-semantic-tokens.el

(autoload 'lsp--semantic-tokens-initialize-buffer "lsp-semantic-tokens" "\
Initialize the buffer for semantic tokens.
IS-RANGE-PROVIDER is non-nil when server supports range requests.

\(fn IS-RANGE-PROVIDER)" nil nil)

(autoload 'lsp--semantic-tokens-initialize-workspace "lsp-semantic-tokens" "\
Initialize semantic tokens for WORKSPACE.

\(fn WORKSPACE)" nil nil)

(autoload 'lsp-semantic-tokens--warn-about-deprecated-setting "lsp-semantic-tokens" "\
Warn about deprecated semantic highlighting variable." nil nil)

(autoload 'lsp-semantic-tokens--enable "lsp-semantic-tokens" "\
Enable semantic tokens mode." nil nil)

(autoload 'lsp-semantic-tokens-mode "lsp-semantic-tokens" "\
Toggle semantic-tokens support.

If called interactively, toggle `Lsp-Semantic-Tokens mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "lsp-semantic-tokens" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-solargraph" "lsp-solargraph.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lsp-solargraph.el

(register-definition-prefixes "lsp-solargraph" '("lsp-solargraph-"))

;;;***

;;;### (autoloads nil "lsp-sorbet" "lsp-sorbet.el" (0 0 0 0))
;;; Generated autoloads from lsp-sorbet.el

(register-definition-prefixes "lsp-sorbet" '("lsp-sorbet-"))

;;;***

;;;### (autoloads nil "lsp-sqls" "lsp-sqls.el" (0 0 0 0))
;;; Generated autoloads from lsp-sqls.el

(register-definition-prefixes "lsp-sqls" '("lsp-sql"))

;;;***

;;;### (autoloads nil "lsp-steep" "lsp-steep.el" (0 0 0 0))
;;; Generated autoloads from lsp-steep.el

(register-definition-prefixes "lsp-steep" '("lsp-steep-"))

;;;***

;;;### (autoloads nil "lsp-svelte" "lsp-svelte.el" (0 0 0 0))
;;; Generated autoloads from lsp-svelte.el

(register-definition-prefixes "lsp-svelte" '("lsp-svelte-plugin-"))

;;;***

;;;### (autoloads nil "lsp-terraform" "lsp-terraform.el" (0 0 0 0))
;;; Generated autoloads from lsp-terraform.el

(register-definition-prefixes "lsp-terraform" '("lsp-terraform-"))

;;;***

;;;### (autoloads nil "lsp-tex" "lsp-tex.el" (0 0 0 0))
;;; Generated autoloads from lsp-tex.el

(register-definition-prefixes "lsp-tex" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-vala" "lsp-vala.el" (0 0 0 0))
;;; Generated autoloads from lsp-vala.el

(register-definition-prefixes "lsp-vala" '("lsp-clients-vala-ls-executable"))

;;;***

;;;### (autoloads nil "lsp-verilog" "lsp-verilog.el" (0 0 0 0))
;;; Generated autoloads from lsp-verilog.el

(register-definition-prefixes "lsp-verilog" '("lsp-clients-verilog-executable"))

;;;***

;;;### (autoloads nil "lsp-vetur" "lsp-vetur.el" (0 0 0 0))
;;; Generated autoloads from lsp-vetur.el

(register-definition-prefixes "lsp-vetur" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-vhdl" "lsp-vhdl.el" (0 0 0 0))
;;; Generated autoloads from lsp-vhdl.el

(register-definition-prefixes "lsp-vhdl" '("ghdl-ls-bin-name" "hdl-checker-bin-name" "lsp-vhdl-" "vhdl-"))

;;;***

;;;### (autoloads nil "lsp-vimscript" "lsp-vimscript.el" (0 0 0 0))
;;; Generated autoloads from lsp-vimscript.el

(register-definition-prefixes "lsp-vimscript" '("lsp-clients-vim-"))

;;;***

;;;### (autoloads nil "lsp-xml" "lsp-xml.el" (0 0 0 0))
;;; Generated autoloads from lsp-xml.el

(register-definition-prefixes "lsp-xml" '("lsp-xml-"))

;;;***

;;;### (autoloads nil "lsp-yaml" "lsp-yaml.el" (0 0 0 0))
;;; Generated autoloads from lsp-yaml.el

(register-definition-prefixes "lsp-yaml" '("lsp-yaml-"))

;;;***

;;;### (autoloads nil "lsp-zig" "lsp-zig.el" (0 0 0 0))
;;; Generated autoloads from lsp-zig.el

(register-definition-prefixes "lsp-zig" '("lsp-zig-zls-executable"))

;;;***

;;;### (autoloads nil nil ("lsp-cmake.el" "lsp-d.el" "lsp-dhall.el"
;;;;;;  "lsp-mode-pkg.el" "lsp-nim.el" "lsp.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-mode-autoloads.el ends here
;;; lsp-pyright-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-pyright" "lsp-pyright.el" (0 0 0 0))
;;; Generated autoloads from lsp-pyright.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-pyright" '("lsp-pyright-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-pyright-autoloads.el ends here
;;; lsp-treemacs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-treemacs" "lsp-treemacs.el" (0 0 0 0))
;;; Generated autoloads from lsp-treemacs.el

(autoload 'lsp-treemacs-symbols "lsp-treemacs" "\
Show symbols view." t nil)

(autoload 'lsp-treemacs-java-deps-list "lsp-treemacs" "\
Display error list." t nil)

(autoload 'lsp-treemacs-java-deps-follow "lsp-treemacs" nil t nil)

(defvar lsp-treemacs-sync-mode nil "\
Non-nil if Lsp-Treemacs-Sync mode is enabled.
See the `lsp-treemacs-sync-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lsp-treemacs-sync-mode'.")

(custom-autoload 'lsp-treemacs-sync-mode "lsp-treemacs" nil)

(autoload 'lsp-treemacs-sync-mode "lsp-treemacs" "\
Global minor mode for synchronizing lsp-mode workspace folders and treemacs projects.

If called interactively, enable Lsp-Treemacs-Sync mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'lsp-treemacs-references "lsp-treemacs" "\
Show the references for the symbol at point.
With a prefix argument, select the new window and expand the tree of references automatically.

\(fn ARG)" t nil)

(autoload 'lsp-treemacs-implementations "lsp-treemacs" "\
Show the implementations for the symbol at point.
With a prefix argument, select the new window expand the tree of implementations automatically.

\(fn ARG)" t nil)

(autoload 'lsp-treemacs-call-hierarchy "lsp-treemacs" "\
Show the incoming call hierarchy for the symbol at point.
With a prefix argument, show the outgoing call hierarchy.

\(fn OUTGOING)" t nil)

(autoload 'lsp-treemacs-errors-list "lsp-treemacs" nil t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-treemacs" '("lsp-tree")))

;;;***

;;;### (autoloads nil "lsp-treemacs-themes" "lsp-treemacs-themes.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-treemacs-themes.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-treemacs-themes" '("lsp-treemacs-theme")))

;;;***

;;;### (autoloads nil nil ("lsp-treemacs-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-treemacs-autoloads.el ends here
;;; lsp-ui-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-ui" "lsp-ui.el" (0 0 0 0))
;;; Generated autoloads from lsp-ui.el

(autoload 'lsp-ui-mode "lsp-ui" "\
Toggle language server UI mode on or off.
‘lsp-ui-mode’ is a minor mode that contains a series of useful UI
integrations for ‘lsp-mode’.  With a prefix argument ARG, enable
language server UI mode if ARG is positive, and disable it
otherwise.  If called from Lisp, enable the mode if ARG is
omitted or nil, and toggle it if ARG is ‘toggle’.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "lsp-ui" '("lsp-ui-"))

;;;***

;;;### (autoloads nil "lsp-ui-doc" "lsp-ui-doc.el" (0 0 0 0))
;;; Generated autoloads from lsp-ui-doc.el

(register-definition-prefixes "lsp-ui-doc" '("lsp-ui-doc-"))

;;;***

;;;### (autoloads nil "lsp-ui-flycheck" "lsp-ui-flycheck.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from lsp-ui-flycheck.el

(register-definition-prefixes "lsp-ui-flycheck" '("lsp-ui-flycheck-"))

;;;***

;;;### (autoloads nil "lsp-ui-imenu" "lsp-ui-imenu.el" (0 0 0 0))
;;; Generated autoloads from lsp-ui-imenu.el

(register-definition-prefixes "lsp-ui-imenu" '("lsp-ui-imenu"))

;;;***

;;;### (autoloads nil "lsp-ui-peek" "lsp-ui-peek.el" (0 0 0 0))
;;; Generated autoloads from lsp-ui-peek.el

(register-definition-prefixes "lsp-ui-peek" '("lsp-"))

;;;***

;;;### (autoloads nil "lsp-ui-sideline" "lsp-ui-sideline.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from lsp-ui-sideline.el

(register-definition-prefixes "lsp-ui-sideline" '("lsp-ui-sideline"))

;;;***

;;;### (autoloads nil "lsp-ui-util" "lsp-ui-util.el" (0 0 0 0))
;;; Generated autoloads from lsp-ui-util.el

(register-definition-prefixes "lsp-ui-util" '("lsp-ui-util-"))

;;;***

;;;### (autoloads nil nil ("lsp-ui-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-ui-autoloads.el ends here
;;; lua-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "init-tryout" "init-tryout.el" (0 0 0 0))
;;; Generated autoloads from init-tryout.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "init-tryout" '("add-trace-for")))

;;;***

;;;### (autoloads nil "lua-mode" "lua-mode.el" (0 0 0 0))
;;; Generated autoloads from lua-mode.el

(autoload 'lua-mode "lua-mode" "\
Major mode for editing Lua code.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(defalias 'run-lua #'lua-start-process)

(autoload 'lua-start-process "lua-mode" "\
Start a Lua process named NAME, running PROGRAM.
PROGRAM defaults to NAME, which defaults to `lua-default-application'.
When called interactively, switch to the process buffer.

\(fn &optional NAME PROGRAM STARTFILE &rest SWITCHES)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lua-mode" '("lua-")))

;;;***

;;;### (autoloads nil nil ("lua-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lua-mode-autoloads.el ends here
;;; lv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lv" "lv.el" (0 0 0 0))
;;; Generated autoloads from lv.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lv" '("lv-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lv-autoloads.el ends here
;;; magit-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "git-rebase" "git-rebase.el" (0 0 0 0))
;;; Generated autoloads from git-rebase.el

(autoload 'git-rebase-current-line "git-rebase" "\
Parse current line into a `git-rebase-action' instance.
If the current line isn't recognized as a rebase line, an
instance with all nil values is returned." nil nil)

(autoload 'git-rebase-mode "git-rebase" "\
Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

\(fn)" t nil)

(defconst git-rebase-filename-regexp "/git-rebase-todo\\'")

(add-to-list 'auto-mode-alist (cons git-rebase-filename-regexp 'git-rebase-mode))

(register-definition-prefixes "git-rebase" '("git-rebase-"))

;;;***

;;;### (autoloads nil "magit" "magit.el" (0 0 0 0))
;;; Generated autoloads from magit.el

(define-obsolete-variable-alias 'global-magit-file-mode 'magit-define-global-key-bindings "Magit 3.0.0")

(defvar magit-define-global-key-bindings t "\
Whether to bind some Magit commands in the global keymap.

If this variable is non-nil, then the following bindings may
be added to the global keymap.  The default is t.

key             binding
---             -------
C-x g           magit-status
C-x M-g         magit-dispatch
C-c M-g         magit-file-dispatch

These bindings may be added when `after-init-hook' is called.
Each binding is added if and only if at that time no other key
is bound to the same command and no other command is bound to
the same key.  In other words we try to avoid adding bindings
that are unnecessary, as well as bindings that conflict with
other bindings.

Adding the above bindings is delayed until `after-init-hook'
is called to allow users to set the variable anywhere in their
init file (without having to make sure to do so before `magit'
is loaded or autoloaded) and to increase the likelihood that
all the potentially conflicting user bindings have already
been added.

Setting this variable after the hook has already been called
has no effect.

We recommend that you bind \"C-c g\" instead of \"C-c M-g\" to
`magit-file-dispatch'.  The former is a much better binding
but the \"C-c <letter>\" namespace is strictly reserved for
users; preventing Magit from using it by default.

Also see info node `(magit)Commands for Buffers Visiting Files'.")

(custom-autoload 'magit-define-global-key-bindings "magit" t)

(defun magit-maybe-define-global-key-bindings nil (when magit-define-global-key-bindings (let ((map (current-global-map))) (dolist (elt '(("C-x g" . magit-status) ("C-x M-g" . magit-dispatch) ("C-c M-g" . magit-file-dispatch))) (let ((key (kbd (car elt))) (def (cdr elt))) (unless (or (lookup-key map key) (where-is-internal def (make-sparse-keymap) t)) (define-key map key def)))))))

(if after-init-time (magit-maybe-define-global-key-bindings) (add-hook 'after-init-hook 'magit-maybe-define-global-key-bindings t))
 (autoload 'magit-dispatch "magit" nil t)
 (autoload 'magit-run "magit" nil t)

(autoload 'magit-git-command "magit" "\
Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

With a prefix argument COMMAND is run in the top-level directory
of the current working tree, otherwise in `default-directory'.

\(fn COMMAND)" t nil)

(autoload 'magit-git-command-topdir "magit" "\
Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

COMMAND is run in the top-level directory of the current
working tree.

\(fn COMMAND)" t nil)

(autoload 'magit-shell-command "magit" "\
Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  With a
prefix argument COMMAND is run in the top-level directory of
the current working tree, otherwise in `default-directory'.

\(fn COMMAND)" t nil)

(autoload 'magit-shell-command-topdir "magit" "\
Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  COMMAND
is run in the top-level directory of the current working tree.

\(fn COMMAND)" t nil)

(autoload 'magit-version "magit" "\
Return the version of Magit currently in use.
If optional argument PRINT-DEST is non-nil, output
stream (interactively, the echo area, or the current buffer with
a prefix argument), also print the used versions of Magit, Git,
and Emacs to it.

\(fn &optional PRINT-DEST)" t nil)

(register-definition-prefixes "magit" '("magit-"))

;;;***

;;;### (autoloads nil "magit-apply" "magit-apply.el" (0 0 0 0))
;;; Generated autoloads from magit-apply.el

(autoload 'magit-stage-file "magit-apply" "\
Stage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be staged.  Otherwise stage the file at point without
requiring confirmation.

\(fn FILE)" t nil)

(autoload 'magit-stage-modified "magit-apply" "\
Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files.

\(fn &optional ALL)" t nil)

(autoload 'magit-unstage-file "magit-apply" "\
Unstage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be unstaged.  Otherwise unstage the file at point
without requiring confirmation.

\(fn FILE)" t nil)

(autoload 'magit-unstage-all "magit-apply" "\
Remove all changes from the staging area." t nil)

(register-definition-prefixes "magit-apply" '("magit-"))

;;;***

;;;### (autoloads nil "magit-autorevert" "magit-autorevert.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from magit-autorevert.el

(put 'magit-auto-revert-mode 'globalized-minor-mode t)

(defvar magit-auto-revert-mode (not (or global-auto-revert-mode noninteractive)) "\
Non-nil if Magit-Auto-Revert mode is enabled.
See the `magit-auto-revert-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-auto-revert-mode'.")

(custom-autoload 'magit-auto-revert-mode "magit-autorevert" nil)

(autoload 'magit-auto-revert-mode "magit-autorevert" "\
Toggle Auto-Revert mode in all buffers.
With prefix ARG, enable Magit-Auto-Revert mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG is
omitted or nil.

Auto-Revert mode is enabled in all buffers where
`magit-turn-on-auto-revert-mode-if-desired' would do it.

See `auto-revert-mode' for more information on Auto-Revert mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "magit-autorevert" '("auto-revert-buffer" "magit-"))

;;;***

;;;### (autoloads nil "magit-bisect" "magit-bisect.el" (0 0 0 0))
;;; Generated autoloads from magit-bisect.el
 (autoload 'magit-bisect "magit-bisect" nil t)

(autoload 'magit-bisect-start "magit-bisect" "\
Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a known
good and a known bad commit.  To move the session forward use the
other actions from the bisect transient command (\\<magit-status-mode-map>\\[magit-bisect]).

\(fn BAD GOOD ARGS)" t nil)

(autoload 'magit-bisect-reset "magit-bisect" "\
After bisecting, cleanup bisection state and return to original `HEAD'." t nil)

(autoload 'magit-bisect-good "magit-bisect" "\
While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question." t nil)

(autoload 'magit-bisect-bad "magit-bisect" "\
While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question." t nil)

(autoload 'magit-bisect-mark "magit-bisect" "\
While bisecting, mark the current commit with a bisect term.
During a bisect using alternate terms, commits can still be
marked with `magit-bisect-good' and `magit-bisect-bad', as those
commands map to the correct term (\"good\" to --term-old's value
and \"bad\" to --term-new's).  However, in some cases, it can be
difficult to keep that mapping straight in your head; this
command provides an interface that exposes the underlying terms." t nil)

(autoload 'magit-bisect-skip "magit-bisect" "\
While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one." t nil)

(autoload 'magit-bisect-run "magit-bisect" "\
Bisect automatically by running commands after each step.

Unlike `git bisect run' this can be used before bisecting has
begun.  In that case it behaves like `git bisect start; git
bisect run'.

\(fn CMDLINE &optional BAD GOOD ARGS)" t nil)

(register-definition-prefixes "magit-bisect" '("magit-"))

;;;***

;;;### (autoloads nil "magit-blame" "magit-blame.el" (0 0 0 0))
;;; Generated autoloads from magit-blame.el
 (autoload 'magit-blame-echo "magit-blame" nil t)
 (autoload 'magit-blame-addition "magit-blame" nil t)
 (autoload 'magit-blame-removal "magit-blame" nil t)
 (autoload 'magit-blame-reverse "magit-blame" nil t)
 (autoload 'magit-blame "magit-blame" nil t)

(register-definition-prefixes "magit-blame" '("magit-"))

;;;***

;;;### (autoloads nil "magit-bookmark" "magit-bookmark.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from magit-bookmark.el

(autoload 'magit--handle-bookmark "magit-bookmark" "\
Open a bookmark created by `magit--make-bookmark'.
Call the `magit-*-setup-buffer' function of the the major-mode
with the variables' values as arguments, which were recorded by
`magit--make-bookmark'.  Ignore `magit-display-buffer-function'.

\(fn BOOKMARK)" nil nil)

(register-definition-prefixes "magit-bookmark" '("magit--make-bookmark"))

;;;***

;;;### (autoloads nil "magit-branch" "magit-branch.el" (0 0 0 0))
;;; Generated autoloads from magit-branch.el
 (autoload 'magit-branch "magit" nil t)

(autoload 'magit-checkout "magit-branch" "\
Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch, then that becomes the current
branch.  If it is something else, then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.

\(git checkout REVISION).

\(fn REVISION &optional ARGS)" t nil)

(autoload 'magit-branch-create "magit-branch" "\
Create BRANCH at branch or revision START-POINT.

\(fn BRANCH START-POINT)" t nil)

(autoload 'magit-branch-and-checkout "magit-branch" "\
Create and checkout BRANCH at branch or revision START-POINT.

\(fn BRANCH START-POINT &optional ARGS)" t nil)

(autoload 'magit-branch-or-checkout "magit-branch" "\
Hybrid between `magit-checkout' and `magit-branch-and-checkout'.

Ask the user for an existing branch or revision.  If the user
input actually can be resolved as a branch or revision, then
check that out, just like `magit-checkout' would.

Otherwise create and checkout a new branch using the input as
its name.  Before doing so read the starting-point for the new
branch.  This is similar to what `magit-branch-and-checkout'
does.

\(fn ARG &optional START-POINT)" t nil)

(autoload 'magit-branch-checkout "magit-branch" "\
Checkout an existing or new local branch.

Read a branch name from the user offering all local branches and
a subset of remote branches as candidates.  Omit remote branches
for which a local branch by the same name exists from the list
of candidates.  The user can also enter a completely new branch
name.

- If the user selects an existing local branch, then check that
  out.

- If the user selects a remote branch, then create and checkout
  a new local branch with the same name.  Configure the selected
  remote branch as push target.

- If the user enters a new branch name, then create and check
  that out, after also reading the starting-point from the user.

In the latter two cases the upstream is also set.  Whether it is
set to the chosen START-POINT or something else depends on the
value of `magit-branch-adjust-remote-upstream-alist', just like
when using `magit-branch-and-checkout'.

\(fn BRANCH &optional START-POINT)" t nil)

(autoload 'magit-branch-orphan "magit-branch" "\
Create and checkout an orphan BRANCH with contents from revision START-POINT.

\(fn BRANCH START-POINT)" t nil)

(autoload 'magit-branch-spinout "magit-branch" "\
Create new branch from the unpushed commits.
Like `magit-branch-spinoff' but remain on the current branch.
If there are any uncommitted changes, then behave exactly like
`magit-branch-spinoff'.

\(fn BRANCH &optional FROM)" t nil)

(autoload 'magit-branch-spinoff "magit-branch" "\
Create new branch from the unpushed commits.

Create and checkout a new branch starting at and tracking the
current branch.  That branch in turn is reset to the last commit
it shares with its upstream.  If the current branch has no
upstream or no unpushed commits, then the new branch is created
anyway and the previously current branch is not touched.

This is useful to create a feature branch after work has already
began on the old branch (likely but not necessarily \"master\").

If the current branch is a member of the value of option
`magit-branch-prefer-remote-upstream' (which see), then the
current branch will be used as the starting point as usual, but
the upstream of the starting-point may be used as the upstream
of the new branch, instead of the starting-point itself.

If optional FROM is non-nil, then the source branch is reset
to `FROM~', instead of to the last commit it shares with its
upstream.  Interactively, FROM is only ever non-nil, if the
region selects some commits, and among those commits, FROM is
the commit that is the fewest commits ahead of the source
branch.

The commit at the other end of the selection actually does not
matter, all commits between FROM and `HEAD' are moved to the new
branch.  If FROM is not reachable from `HEAD' or is reachable
from the source branch's upstream, then an error is raised.

\(fn BRANCH &optional FROM)" t nil)

(autoload 'magit-branch-reset "magit-branch" "\
Reset a branch to the tip of another branch or any other commit.

When the branch being reset is the current branch, then do a
hard reset.  If there are any uncommitted changes, then the user
has to confirm the reset because those changes would be lost.

This is useful when you have started work on a feature branch but
realize it's all crap and want to start over.

When resetting to another branch and a prefix argument is used,
then also set the target branch as the upstream of the branch
that is being reset.

\(fn BRANCH TO &optional SET-UPSTREAM)" t nil)

(autoload 'magit-branch-delete "magit-branch" "\
Delete one or multiple branches.
If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point.

\(fn BRANCHES &optional FORCE)" t nil)

(autoload 'magit-branch-rename "magit-branch" "\
Rename the branch named OLD to NEW.

With a prefix argument FORCE, rename even if a branch named NEW
already exists.

If `branch.OLD.pushRemote' is set, then unset it.  Depending on
the value of `magit-branch-rename-push-target' (which see) maybe
set `branch.NEW.pushRemote' and maybe rename the push-target on
the remote.

\(fn OLD NEW &optional FORCE)" t nil)

(autoload 'magit-branch-shelve "magit-branch" "\
Shelve a BRANCH.
Rename \"refs/heads/BRANCH\" to \"refs/shelved/BRANCH\",
and also rename the respective reflog file.

\(fn BRANCH)" t nil)

(autoload 'magit-branch-unshelve "magit-branch" "\
Unshelve a BRANCH
Rename \"refs/shelved/BRANCH\" to \"refs/heads/BRANCH\",
and also rename the respective reflog file.

\(fn BRANCH)" t nil)
 (autoload 'magit-branch-configure "magit-branch" nil t)

(register-definition-prefixes "magit-branch" '("magit-"))

;;;***

;;;### (autoloads nil "magit-clone" "magit-clone.el" (0 0 0 0))
;;; Generated autoloads from magit-clone.el
 (autoload 'magit-clone "magit-clone" nil t)

(autoload 'magit-clone-regular "magit-clone" "\
Create a clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

\(fn REPOSITORY DIRECTORY ARGS)" t nil)

(autoload 'magit-clone-shallow "magit-clone" "\
Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
With a prefix argument read the DEPTH of the clone;
otherwise use 1.

\(fn REPOSITORY DIRECTORY ARGS DEPTH)" t nil)

(autoload 'magit-clone-shallow-since "magit-clone" "\
Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits before DATE, which is read from the
user.

\(fn REPOSITORY DIRECTORY ARGS DATE)" t nil)

(autoload 'magit-clone-shallow-exclude "magit-clone" "\
Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits reachable from EXCLUDE, which is a
branch or tag read from the user.

\(fn REPOSITORY DIRECTORY ARGS EXCLUDE)" t nil)

(autoload 'magit-clone-bare "magit-clone" "\
Create a bare clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

\(fn REPOSITORY DIRECTORY ARGS)" t nil)

(autoload 'magit-clone-mirror "magit-clone" "\
Create a mirror of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

\(fn REPOSITORY DIRECTORY ARGS)" t nil)

(register-definition-prefixes "magit-clone" '("magit-clone-"))

;;;***

;;;### (autoloads nil "magit-commit" "magit-commit.el" (0 0 0 0))
;;; Generated autoloads from magit-commit.el
 (autoload 'magit-commit "magit-commit" nil t)

(autoload 'magit-commit-create "magit-commit" "\
Create a new commit on `HEAD'.
With a prefix argument, amend to the commit at `HEAD' instead.

\(git commit [--amend] ARGS)

\(fn &optional ARGS)" t nil)

(autoload 'magit-commit-amend "magit-commit" "\
Amend the last commit.

\(git commit --amend ARGS)

\(fn &optional ARGS)" t nil)

(autoload 'magit-commit-extend "magit-commit" "\
Amend the last commit, without editing the message.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-extend-override-date' can be used
to inverse the meaning of the prefix argument.  
\(git commit
--amend --no-edit)

\(fn &optional ARGS OVERRIDE-DATE)" t nil)

(autoload 'magit-commit-reword "magit-commit" "\
Reword the last commit, ignoring staged changes.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-reword-override-date' can be used
to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.

\(git commit --amend --only)

\(fn &optional ARGS OVERRIDE-DATE)" t nil)

(autoload 'magit-commit-fixup "magit-commit" "\
Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

\(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-squash "magit-commit" "\
Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

\(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-augment "magit-commit" "\
Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

\(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-instant-fixup "magit-commit" "\
Create a fixup commit targeting COMMIT and instantly rebase.

\(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-instant-squash "magit-commit" "\
Create a squash commit targeting COMMIT and instantly rebase.

\(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-reshelve "magit-commit" "\
Change the committer date and possibly the author date of `HEAD'.

The current time is used as the initial minibuffer input and the
original author or committer date is available as the previous
history element.

Both the author and the committer dates are changes, unless one
of the following is true, in which case only the committer date
is updated:
- You are not the author of the commit that is being reshelved.
- The command was invoked with a prefix argument.
- Non-interactively if UPDATE-AUTHOR is nil.

\(fn DATE UPDATE-AUTHOR &optional ARGS)" t nil)

(autoload 'magit-commit-absorb-modules "magit-commit" "\
Spread modified modules across recent commits.

\(fn PHASE COMMIT)" t nil)
 (autoload 'magit-commit-absorb "magit-commit" nil t)
 (autoload 'magit-commit-autofixup "magit-commit" nil t)

(register-definition-prefixes "magit-commit" '("magit-"))

;;;***

;;;### (autoloads nil "magit-diff" "magit-diff.el" (0 0 0 0))
;;; Generated autoloads from magit-diff.el
 (autoload 'magit-diff "magit-diff" nil t)
 (autoload 'magit-diff-refresh "magit-diff" nil t)

(autoload 'magit-diff-dwim "magit-diff" "\
Show changes for the thing at point.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff-range "magit-diff" "\
Show differences between two commits.

REV-OR-RANGE should be a range or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range).

\(fn REV-OR-RANGE &optional ARGS FILES)" t nil)

(autoload 'magit-diff-working-tree "magit-diff" "\
Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer.

\(fn &optional REV ARGS FILES)" t nil)

(autoload 'magit-diff-staged "magit-diff" "\
Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer.

\(fn &optional REV ARGS FILES)" t nil)

(autoload 'magit-diff-unstaged "magit-diff" "\
Show changes between the working tree and the index.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff-unmerged "magit-diff" "\
Show changes that are being merged.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff-while-committing "magit-diff" "\
While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be committed.

\(fn &optional ARGS)" t nil)

(autoload 'magit-diff-buffer-file "magit-diff" "\
Show diff for the blob or file visited in the current buffer.

When the buffer visits a blob, then show the respective commit.
When the buffer visits a file, then show the differenced between
`HEAD' and the working tree.  In both cases limit the diff to
the file or blob." t nil)

(autoload 'magit-diff-paths "magit-diff" "\
Show changes between any two files on disk.

\(fn A B)" t nil)

(autoload 'magit-show-commit "magit-diff" "\
Visit the revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision.

\(fn REV &optional ARGS FILES MODULE)" t nil)

(register-definition-prefixes "magit-diff" '("magit-"))

;;;***

;;;### (autoloads nil "magit-ediff" "magit-ediff.el" (0 0 0 0))
;;; Generated autoloads from magit-ediff.el
 (autoload 'magit-ediff "magit-ediff" nil)

(autoload 'magit-ediff-resolve "magit-ediff" "\
Resolve outstanding conflicts in FILE using Ediff.
FILE has to be relative to the top directory of the repository.

In the rare event that you want to manually resolve all
conflicts, including those already resolved by Git, use
`ediff-merge-revisions-with-ancestor'.

\(fn FILE)" t nil)

(autoload 'magit-ediff-stage "magit-ediff" "\
Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'magit-ediff-compare "magit-ediff" "\
Compare REVA:FILEA with REVB:FILEB using Ediff.

FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil, then this stands for the
working tree state.

If the region is active, use the revisions on the first and last
line of the region.  With a prefix argument, instead of diffing
the revisions, choose a revision to view changes along, starting
at the common ancestor of both revisions (i.e., use a \"...\"
range).

\(fn REVA REVB FILEA FILEB)" t nil)

(autoload 'magit-ediff-dwim "magit-ediff" "\
Compare, stage, or resolve using Ediff.
This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using Ediff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `magit-ediff-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run." t nil)

(autoload 'magit-ediff-show-staged "magit-ediff" "\
Show staged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'magit-ediff-show-unstaged "magit-ediff" "\
Show unstaged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'magit-ediff-show-working-tree "magit-ediff" "\
Show changes between `HEAD' and working tree using Ediff.
FILE must be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'magit-ediff-show-commit "magit-ediff" "\
Show changes introduced by COMMIT using Ediff.

\(fn COMMIT)" t nil)

(autoload 'magit-ediff-show-stash "magit-ediff" "\
Show changes introduced by STASH using Ediff.
`magit-ediff-show-stash-with-index' controls whether a
three-buffer Ediff is used in order to distinguish changes in the
stash that were staged.

\(fn STASH)" t nil)

(register-definition-prefixes "magit-ediff" '("magit-ediff-"))

;;;***

;;;### (autoloads nil "magit-extras" "magit-extras.el" (0 0 0 0))
;;; Generated autoloads from magit-extras.el

(autoload 'magit-run-git-gui "magit-extras" "\
Run `git gui' for the current git repository." t nil)

(autoload 'magit-run-git-gui-blame "magit-extras" "\
Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the `HEAD', with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on.

\(fn COMMIT FILENAME &optional LINENUM)" t nil)

(autoload 'magit-run-gitk "magit-extras" "\
Run `gitk' in the current repository." t nil)

(autoload 'magit-run-gitk-branches "magit-extras" "\
Run `gitk --branches' in the current repository." t nil)

(autoload 'magit-run-gitk-all "magit-extras" "\
Run `gitk --all' in the current repository." t nil)

(autoload 'ido-enter-magit-status "magit-extras" "\
Drop into `magit-status' from file switching.

This command does not work in Emacs 26.1.
See https://github.com/magit/magit/issues/3634
and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31707.

To make this command available use something like:

  (add-hook \\='ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd \"C-x g\") \\='ido-enter-magit-status)))

Starting with Emacs 25.1 the Ido keymaps are defined just once
instead of every time Ido is invoked, so now you can modify it
like pretty much every other keymap:

  (define-key ido-common-completion-map
    (kbd \"C-x g\") \\='ido-enter-magit-status)" t nil)

(autoload 'magit-project-status "magit-extras" "\
Run `magit-status' in the current project's root." t nil)

(autoload 'magit-dired-jump "magit-extras" "\
Visit file at point using Dired.
With a prefix argument, visit in another window.  If there
is no file at point, then instead visit `default-directory'.

\(fn &optional OTHER-WINDOW)" t nil)

(autoload 'magit-dired-log "magit-extras" "\
Show log for all marked files, or the current file.

\(fn &optional FOLLOW)" t nil)

(autoload 'magit-do-async-shell-command "magit-extras" "\
Open FILE with `dired-do-async-shell-command'.
Interactively, open the file at point.

\(fn FILE)" t nil)

(autoload 'magit-previous-line "magit-extras" "\
Like `previous-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects an
area that is larger than the region.  This causes `previous-line'
when invoked while holding the shift key to move up one line and
thereby select two lines.  When invoked inside a hunk body this
command does not move point on the first invocation and thereby
it only selects a single line.  Which inconsistency you prefer
is a matter of preference.

\(fn &optional ARG TRY-VSCROLL)" t nil)

(function-put 'magit-previous-line 'interactive-only '"use `forward-line' with negative argument instead.")

(autoload 'magit-next-line "magit-extras" "\
Like `next-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects
an area that is larger than the region.  This causes `next-line'
when invoked while holding the shift key to move down one line
and thereby select two lines.  When invoked inside a hunk body
this command does not move point on the first invocation and
thereby it only selects a single line.  Which inconsistency you
prefer is a matter of preference.

\(fn &optional ARG TRY-VSCROLL)" t nil)

(function-put 'magit-next-line 'interactive-only 'forward-line)

(autoload 'magit-clean "magit-extras" "\
Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.

\(git clean -f -d [-x|-X])

\(fn &optional ARG)" t nil)

(autoload 'magit-add-change-log-entry "magit-extras" "\
Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer.

\(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t nil)

(autoload 'magit-add-change-log-entry-other-window "magit-extras" "\
Find change log file in other window and add entry and item.
This differs from `add-change-log-entry-other-window' (which see)
in that it acts on the current hunk in a Magit buffer instead of
on a position in a file-visiting buffer.

\(fn &optional WHOAMI FILE-NAME)" t nil)

(autoload 'magit-edit-line-commit "magit-extras" "\
Edit the commit that added the current line.

With a prefix argument edit the commit that removes the line,
if any.  The commit is determined using `git blame' and made
editable using `git rebase --interactive' if it is reachable
from `HEAD', or by checking out the commit (or a branch that
points at it) otherwise.

\(fn &optional TYPE)" t nil)

(autoload 'magit-diff-edit-hunk-commit "magit-extras" "\
From a hunk, edit the respective commit and visit the file.

First visit the file being modified by the hunk at the correct
location using `magit-diff-visit-file'.  This actually visits a
blob.  When point is on a diff header, not within an individual
hunk, then this visits the blob the first hunk is about.

Then invoke `magit-edit-line-commit', which uses an interactive
rebase to make the commit editable, or if that is not possible
because the commit is not reachable from `HEAD' by checking out
that commit directly.  This also causes the actual worktree file
to be visited.

Neither the blob nor the file buffer are killed when finishing
the rebase.  If that is undesirable, then it might be better to
use `magit-rebase-edit-command' instead of this command.

\(fn FILE)" t nil)

(autoload 'magit-reshelve-since "magit-extras" "\
Change the author and committer dates of the commits since REV.

Ask the user for the first reachable commit whose dates should
be changed.  Then read the new date for that commit.  The initial
minibuffer input and the previous history element offer good
values.  The next commit will be created one minute later and so
on.

This command is only intended for interactive use and should only
be used on highly rearranged and unpublished history.

If KEYID is non-nil, then use that to sign all reshelved commits.
Interactively use the value of the \"--gpg-sign\" option in the
list returned by `magit-rebase-arguments'.

\(fn REV KEYID)" t nil)

(autoload 'magit-pop-revision-stack "magit-extras" "\
Insert a representation of a revision into the current buffer.

Pop a revision from the `magit-revision-stack' and insert it into
the current buffer according to `magit-pop-revision-stack-format'.
Revisions can be put on the stack using `magit-copy-section-value'
and `magit-copy-buffer-revision'.

If the stack is empty or with a prefix argument, instead read a
revision in the minibuffer.  By using the minibuffer history this
allows selecting an item which was popped earlier or to insert an
arbitrary reference or revision without first pushing it onto the
stack.

When reading the revision from the minibuffer, then it might not
be possible to guess the correct repository.  When this command
is called inside a repository (e.g. while composing a commit
message), then that repository is used.  Otherwise (e.g. while
composing an email) then the repository recorded for the top
element of the stack is used (even though we insert another
revision).  If not called inside a repository and with an empty
stack, or with two prefix arguments, then read the repository in
the minibuffer too.

\(fn REV TOPLEVEL)" t nil)

(autoload 'magit-copy-section-value "magit-extras" "\
Save the value of the current section for later use.

Save the section value to the `kill-ring', and, provided that
the current section is a commit, branch, or tag section, push
the (referenced) revision to the `magit-revision-stack' for use
with `magit-pop-revision-stack'.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'.

When the current section is a branch or a tag, and a prefix
argument is used, then save the revision at its tip to the
`kill-ring' instead of the reference name.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.  If a prefix argument is used and the region is within
a hunk, then strip the diff marker column and keep only either
the added or removed lines, depending on the sign of the prefix
argument.

\(fn ARG)" t nil)

(autoload 'magit-copy-buffer-revision "magit-extras" "\
Save the revision of the current buffer for later use.

Save the revision shown in the current buffer to the `kill-ring'
and push it to the `magit-revision-stack'.

This command is mainly intended for use in `magit-revision-mode'
buffers, the only buffers where it is always unambiguous exactly
which revision should be saved.

Most other Magit buffers usually show more than one revision, in
some way or another, so this command has to select one of them,
and that choice might not always be the one you think would have
been the best pick.

In such buffers it is often more useful to save the value of
the current section instead, using `magit-copy-section-value'.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'." t nil)

(autoload 'magit-abort-dwim "magit-extras" "\
Abort current operation.
Depending on the context, this will abort a merge, a rebase, a
patch application, a cherry-pick, a revert, or a bisect." t nil)

(register-definition-prefixes "magit-extras" '("magit-"))

;;;***

;;;### (autoloads nil "magit-fetch" "magit-fetch.el" (0 0 0 0))
;;; Generated autoloads from magit-fetch.el
 (autoload 'magit-fetch "magit-fetch" nil t)
 (autoload 'magit-fetch-from-pushremote "magit-fetch" nil t)
 (autoload 'magit-fetch-from-upstream "magit-fetch" nil t)

(autoload 'magit-fetch-other "magit-fetch" "\
Fetch from another repository.

\(fn REMOTE ARGS)" t nil)

(autoload 'magit-fetch-branch "magit-fetch" "\
Fetch a BRANCH from a REMOTE.

\(fn REMOTE BRANCH ARGS)" t nil)

(autoload 'magit-fetch-refspec "magit-fetch" "\
Fetch a REFSPEC from a REMOTE.

\(fn REMOTE REFSPEC ARGS)" t nil)

(autoload 'magit-fetch-all "magit-fetch" "\
Fetch from all remotes.

\(fn ARGS)" t nil)

(autoload 'magit-fetch-all-prune "magit-fetch" "\
Fetch from all remotes, and prune.
Prune remote tracking branches for branches that have been
removed on the respective remote." t nil)

(autoload 'magit-fetch-all-no-prune "magit-fetch" "\
Fetch from all remotes." t nil)

(autoload 'magit-fetch-modules "magit-fetch" "\
Fetch all submodules.

Option `magit-fetch-modules-jobs' controls how many submodules
are being fetched in parallel.  Also fetch the super-repository,
because `git-fetch' does not support not doing that.  With a
prefix argument fetch all remotes.

\(fn &optional ALL)" t nil)

(register-definition-prefixes "magit-fetch" '("magit-"))

;;;***

;;;### (autoloads nil "magit-files" "magit-files.el" (0 0 0 0))
;;; Generated autoloads from magit-files.el

(autoload 'magit-find-file "magit-files" "\
View FILE from REV.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go
to the line and column corresponding to that location.

\(fn REV FILE)" t nil)

(autoload 'magit-find-file-other-window "magit-files" "\
View FILE from REV, in another window.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

\(fn REV FILE)" t nil)

(autoload 'magit-find-file-other-frame "magit-files" "\
View FILE from REV, in another frame.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

\(fn REV FILE)" t nil)
 (autoload 'magit-file-dispatch "magit" nil t)

(autoload 'magit-blob-visit-file "magit-files" "\
View the file from the worktree corresponding to the current blob.
When visiting a blob or the version from the index, then go to
the same location in the respective file in the working tree." t nil)

(autoload 'magit-file-checkout "magit-files" "\
Checkout FILE from REV.

\(fn REV FILE)" t nil)

(register-definition-prefixes "magit-files" '("magit-"))

;;;***

;;;### (autoloads nil "magit-git" "magit-git.el" (0 0 0 0))
;;; Generated autoloads from magit-git.el

(register-definition-prefixes "magit-git" '("magit-"))

;;;***

;;;### (autoloads nil "magit-gitignore" "magit-gitignore.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from magit-gitignore.el
 (autoload 'magit-gitignore "magit-gitignore" nil t)

(autoload 'magit-gitignore-in-topdir "magit-gitignore" "\
Add the Git ignore RULE to the top-level \".gitignore\" file.
Since this file is tracked, it is shared with other clones of the
repository.  Also stage the file.

\(fn RULE)" t nil)

(autoload 'magit-gitignore-in-subdir "magit-gitignore" "\
Add the Git ignore RULE to a \".gitignore\" file in DIRECTORY.
Prompt the user for a directory and add the rule to the
\".gitignore\" file in that directory.  Since such files are
tracked, they are shared with other clones of the repository.
Also stage the file.

\(fn RULE DIRECTORY)" t nil)

(autoload 'magit-gitignore-in-gitdir "magit-gitignore" "\
Add the Git ignore RULE to \"$GIT_DIR/info/exclude\".
Rules in that file only affects this clone of the repository.

\(fn RULE)" t nil)

(autoload 'magit-gitignore-on-system "magit-gitignore" "\
Add the Git ignore RULE to the file specified by `core.excludesFile'.
Rules that are defined in that file affect all local repositories.

\(fn RULE)" t nil)

(autoload 'magit-skip-worktree "magit-gitignore" "\
Call \"git update-index --skip-worktree -- FILE\".

\(fn FILE)" t nil)

(autoload 'magit-no-skip-worktree "magit-gitignore" "\
Call \"git update-index --no-skip-worktree -- FILE\".

\(fn FILE)" t nil)

(autoload 'magit-assume-unchanged "magit-gitignore" "\
Call \"git update-index --assume-unchanged -- FILE\".

\(fn FILE)" t nil)

(autoload 'magit-no-assume-unchanged "magit-gitignore" "\
Call \"git update-index --no-assume-unchanged -- FILE\".

\(fn FILE)" t nil)

(register-definition-prefixes "magit-gitignore" '("magit-"))

;;;***

;;;### (autoloads nil "magit-imenu" "magit-imenu.el" (0 0 0 0))
;;; Generated autoloads from magit-imenu.el

(autoload 'magit-imenu--log-prev-index-position-function "magit-imenu" "\
Move point to previous line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil)

(autoload 'magit-imenu--log-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil)

(autoload 'magit-imenu--diff-prev-index-position-function "magit-imenu" "\
Move point to previous file line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil)

(autoload 'magit-imenu--diff-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil)

(autoload 'magit-imenu--status-create-index-function "magit-imenu" "\
Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil)

(autoload 'magit-imenu--refs-create-index-function "magit-imenu" "\
Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil)

(autoload 'magit-imenu--cherry-create-index-function "magit-imenu" "\
Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil)

(autoload 'magit-imenu--submodule-prev-index-position-function "magit-imenu" "\
Move point to previous line in magit-submodule-list buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil)

(autoload 'magit-imenu--submodule-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil)

(autoload 'magit-imenu--repolist-prev-index-position-function "magit-imenu" "\
Move point to previous line in magit-repolist buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil)

(autoload 'magit-imenu--repolist-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil)

(autoload 'magit-imenu--process-prev-index-position-function "magit-imenu" "\
Move point to previous process in magit-process buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil)

(autoload 'magit-imenu--process-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil)

(autoload 'magit-imenu--rebase-prev-index-position-function "magit-imenu" "\
Move point to previous commit in git-rebase buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil)

(autoload 'magit-imenu--rebase-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil)

(register-definition-prefixes "magit-imenu" '("magit-imenu--index-function"))

;;;***

;;;### (autoloads nil "magit-log" "magit-log.el" (0 0 0 0))
;;; Generated autoloads from magit-log.el
 (autoload 'magit-log "magit-log" nil t)
 (autoload 'magit-log-refresh "magit-log" nil t)

(autoload 'magit-log-current "magit-log" "\
Show log for the current branch.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer.

\(fn REVS &optional ARGS FILES)" t nil)

(autoload 'magit-log-other "magit-log" "\
Show log for one or more revs read from the minibuffer.
The user can input any revision or revisions separated by a
space, or even ranges, but only branches and tags, and a
representation of the commit at point, are available as
completion candidates.

\(fn REVS &optional ARGS FILES)" t nil)

(autoload 'magit-log-head "magit-log" "\
Show log for `HEAD'.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-branches "magit-log" "\
Show log for all local branches and `HEAD'.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-matching-branches "magit-log" "\
Show log for all branches matching PATTERN and `HEAD'.

\(fn PATTERN &optional ARGS FILES)" t nil)

(autoload 'magit-log-matching-tags "magit-log" "\
Show log for all tags matching PATTERN and `HEAD'.

\(fn PATTERN &optional ARGS FILES)" t nil)

(autoload 'magit-log-all-branches "magit-log" "\
Show log for all local and remote branches and `HEAD'.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-all "magit-log" "\
Show log for all references and `HEAD'.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-buffer-file "magit-log" "\
Show log for the blob or file visited in the current buffer.
With a prefix argument or when `--follow' is an active log
argument, then follow renames.  When the region is active,
restrict the log to the lines that the region touches.

\(fn &optional FOLLOW BEG END)" t nil)

(autoload 'magit-log-trace-definition "magit-log" "\
Show log for the definition at point.

\(fn FILE FN REV)" t nil)

(autoload 'magit-log-merged "magit-log" "\
Show log for the merge of COMMIT into BRANCH.

More precisely, find merge commit M that brought COMMIT into
BRANCH, and show the log of the range \"M^1..M\".  If COMMIT is
directly on BRANCH, then show approximately twenty surrounding
commits instead.

This command requires git-when-merged, which is available from
https://github.com/mhagger/git-when-merged.

\(fn COMMIT BRANCH &optional ARGS FILES)" t nil)

(autoload 'magit-log-move-to-parent "magit-log" "\
Move to the Nth parent of the current commit.

\(fn &optional N)" t nil)
 (autoload 'magit-shortlog "magit-log" nil t)

(autoload 'magit-shortlog-since "magit-log" "\
Show a history summary for commits since REV.

\(fn REV ARGS)" t nil)

(autoload 'magit-shortlog-range "magit-log" "\
Show a history summary for commit or range REV-OR-RANGE.

\(fn REV-OR-RANGE ARGS)" t nil)

(autoload 'magit-cherry "magit-log" "\
Show commits in a branch that are not merged in the upstream branch.

\(fn HEAD UPSTREAM)" t nil)

(register-definition-prefixes "magit-log" '("magit-"))

;;;***

;;;### (autoloads nil "magit-margin" "magit-margin.el" (0 0 0 0))
;;; Generated autoloads from magit-margin.el

(register-definition-prefixes "magit-margin" '("magit-"))

;;;***

;;;### (autoloads nil "magit-merge" "magit-merge.el" (0 0 0 0))
;;; Generated autoloads from magit-merge.el
 (autoload 'magit-merge "magit" nil t)

(autoload 'magit-merge-plain "magit-merge" "\
Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

\(git merge --no-edit|--no-commit [ARGS] REV)

\(fn REV &optional ARGS NOCOMMIT)" t nil)

(autoload 'magit-merge-editmsg "magit-merge" "\
Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.

\(git merge --edit --no-ff [ARGS] REV)

\(fn REV &optional ARGS)" t nil)

(autoload 'magit-merge-nocommit "magit-merge" "\
Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.

\(git merge --no-commit --no-ff [ARGS] REV)

\(fn REV &optional ARGS)" t nil)

(autoload 'magit-merge-into "magit-merge" "\
Merge the current branch into BRANCH and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
branch, then also remove the respective remote branch.

\(fn BRANCH &optional ARGS)" t nil)

(autoload 'magit-merge-absorb "magit-merge" "\
Merge BRANCH into the current branch and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch.

\(fn BRANCH &optional ARGS)" t nil)

(autoload 'magit-merge-squash "magit-merge" "\
Squash commit REV into the current branch; don't create a commit.

\(git merge --squash REV)

\(fn REV)" t nil)

(autoload 'magit-merge-preview "magit-merge" "\
Preview result of merging REV into the current branch.

\(fn REV)" t nil)

(autoload 'magit-merge-abort "magit-merge" "\
Abort the current merge operation.

\(git merge --abort)" t nil)

(register-definition-prefixes "magit-merge" '("magit-"))

;;;***

;;;### (autoloads nil "magit-mode" "magit-mode.el" (0 0 0 0))
;;; Generated autoloads from magit-mode.el

(register-definition-prefixes "magit-mode" '("disable-magit-save-buffers" "magit-"))

;;;***

;;;### (autoloads nil "magit-notes" "magit-notes.el" (0 0 0 0))
;;; Generated autoloads from magit-notes.el
 (autoload 'magit-notes "magit" nil t)

(register-definition-prefixes "magit-notes" '("magit-notes-"))

;;;***

;;;### (autoloads nil "magit-obsolete" "magit-obsolete.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from magit-obsolete.el

(register-definition-prefixes "magit-obsolete" '("magit--magit-popup-warning"))

;;;***

;;;### (autoloads nil "magit-patch" "magit-patch.el" (0 0 0 0))
;;; Generated autoloads from magit-patch.el
 (autoload 'magit-patch "magit-patch" nil t)
 (autoload 'magit-patch-create "magit-patch" nil t)
 (autoload 'magit-patch-apply "magit-patch" nil t)

(autoload 'magit-patch-save "magit-patch" "\
Write current diff into patch FILE.

What arguments are used to create the patch depends on the value
of `magit-patch-save-arguments' and whether a prefix argument is
used.

If the value is the symbol `buffer', then use the same arguments
as the buffer.  With a prefix argument use no arguments.

If the value is a list beginning with the symbol `exclude', then
use the same arguments as the buffer except for those matched by
entries in the cdr of the list.  The comparison is done using
`string-prefix-p'.  With a prefix argument use the same arguments
as the buffer.

If the value is a list of strings (including the empty list),
then use those arguments.  With a prefix argument use the same
arguments as the buffer.

Of course the arguments that are required to actually show the
same differences as those shown in the buffer are always used.

\(fn FILE &optional ARG)" t nil)

(autoload 'magit-request-pull "magit-patch" "\
Request upstream to pull from your public repository.

URL is the url of your publicly accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit.

\(fn URL START END)" t nil)

(register-definition-prefixes "magit-patch" '("magit-"))

;;;***

;;;### (autoloads nil "magit-process" "magit-process.el" (0 0 0 0))
;;; Generated autoloads from magit-process.el

(register-definition-prefixes "magit-process" '("magit-" "tramp-sh-handle-"))

;;;***

;;;### (autoloads nil "magit-pull" "magit-pull.el" (0 0 0 0))
;;; Generated autoloads from magit-pull.el
 (autoload 'magit-pull "magit-pull" nil t)
 (autoload 'magit-pull-from-pushremote "magit-pull" nil t)
 (autoload 'magit-pull-from-upstream "magit-pull" nil t)

(autoload 'magit-pull-branch "magit-pull" "\
Pull from a branch read in the minibuffer.

\(fn SOURCE ARGS)" t nil)

(register-definition-prefixes "magit-pull" '("magit-pull-"))

;;;***

;;;### (autoloads nil "magit-push" "magit-push.el" (0 0 0 0))
;;; Generated autoloads from magit-push.el
 (autoload 'magit-push "magit-push" nil t)
 (autoload 'magit-push-current-to-pushremote "magit-push" nil t)
 (autoload 'magit-push-current-to-upstream "magit-push" nil t)

(autoload 'magit-push-current "magit-push" "\
Push the current branch to a branch read in the minibuffer.

\(fn TARGET ARGS)" t nil)

(autoload 'magit-push-other "magit-push" "\
Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer.

\(fn SOURCE TARGET ARGS)" t nil)

(autoload 'magit-push-refspecs "magit-push" "\
Push one or multiple REFSPECS to a REMOTE.
Both the REMOTE and the REFSPECS are read in the minibuffer.  To
use multiple REFSPECS, separate them with commas.  Completion is
only available for the part before the colon, or when no colon
is used.

\(fn REMOTE REFSPECS ARGS)" t nil)

(autoload 'magit-push-matching "magit-push" "\
Push all matching branches to another repository.
If multiple remotes exist, then read one from the user.
If just one exists, use that without requiring confirmation.

\(fn REMOTE &optional ARGS)" t nil)

(autoload 'magit-push-tags "magit-push" "\
Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default.

\(fn REMOTE &optional ARGS)" t nil)

(autoload 'magit-push-tag "magit-push" "\
Push a tag to another repository.

\(fn TAG REMOTE &optional ARGS)" t nil)

(autoload 'magit-push-notes-ref "magit-push" "\
Push a notes ref to another repository.

\(fn REF REMOTE &optional ARGS)" t nil)
 (autoload 'magit-push-implicitly "magit-push" nil t)

(autoload 'magit-push-to-remote "magit-push" "\
Push to REMOTE without using an explicit refspec.
The REMOTE is read in the minibuffer.

This command simply runs \"git push -v [ARGS] REMOTE\".  ARGS
are the arguments specified in the popup buffer.  No refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

\(fn REMOTE ARGS)" t nil)

(register-definition-prefixes "magit-push" '("magit-"))

;;;***

;;;### (autoloads nil "magit-reflog" "magit-reflog.el" (0 0 0 0))
;;; Generated autoloads from magit-reflog.el

(autoload 'magit-reflog-current "magit-reflog" "\
Display the reflog of the current branch.
If `HEAD' is detached, then show the reflog for that instead." t nil)

(autoload 'magit-reflog-other "magit-reflog" "\
Display the reflog of a branch or another ref.

\(fn REF)" t nil)

(autoload 'magit-reflog-head "magit-reflog" "\
Display the `HEAD' reflog." t nil)

(register-definition-prefixes "magit-reflog" '("magit-reflog-"))

;;;***

;;;### (autoloads nil "magit-refs" "magit-refs.el" (0 0 0 0))
;;; Generated autoloads from magit-refs.el
 (autoload 'magit-show-refs "magit-refs" nil t)

(autoload 'magit-show-refs-head "magit-refs" "\
List and compare references in a dedicated buffer.
Compared with `HEAD'.

\(fn &optional ARGS)" t nil)

(autoload 'magit-show-refs-current "magit-refs" "\
List and compare references in a dedicated buffer.
Compare with the current branch or `HEAD' if it is detached.

\(fn &optional ARGS)" t nil)

(autoload 'magit-show-refs-other "magit-refs" "\
List and compare references in a dedicated buffer.
Compared with a branch read from the user.

\(fn &optional REF ARGS)" t nil)

(register-definition-prefixes "magit-refs" '("magit-"))

;;;***

;;;### (autoloads nil "magit-remote" "magit-remote.el" (0 0 0 0))
;;; Generated autoloads from magit-remote.el
 (autoload 'magit-remote "magit-remote" nil t)

(autoload 'magit-remote-add "magit-remote" "\
Add a remote named REMOTE and fetch it.

\(fn REMOTE URL &optional ARGS)" t nil)

(autoload 'magit-remote-rename "magit-remote" "\
Rename the remote named OLD to NEW.

\(fn OLD NEW)" t nil)

(autoload 'magit-remote-remove "magit-remote" "\
Delete the remote named REMOTE.

\(fn REMOTE)" t nil)

(autoload 'magit-remote-prune "magit-remote" "\
Remove stale remote-tracking branches for REMOTE.

\(fn REMOTE)" t nil)

(autoload 'magit-remote-prune-refspecs "magit-remote" "\
Remove stale refspecs for REMOTE.

A refspec is stale if there no longer exists at least one branch
on the remote that would be fetched due to that refspec.  A stale
refspec is problematic because its existence causes Git to refuse
to fetch according to the remaining non-stale refspecs.

If only stale refspecs remain, then offer to either delete the
remote or to replace the stale refspecs with the default refspec.

Also remove the remote-tracking branches that were created due to
the now stale refspecs.  Other stale branches are not removed.

\(fn REMOTE)" t nil)

(autoload 'magit-remote-set-head "magit-remote" "\
Set the local representation of REMOTE's default branch.
Query REMOTE and set the symbolic-ref refs/remotes/<remote>/HEAD
accordingly.  With a prefix argument query for the branch to be
used, which allows you to select an incorrect value if you fancy
doing that.

\(fn REMOTE &optional BRANCH)" t nil)

(autoload 'magit-remote-unset-head "magit-remote" "\
Unset the local representation of REMOTE's default branch.
Delete the symbolic-ref \"refs/remotes/<remote>/HEAD\".

\(fn REMOTE)" t nil)
 (autoload 'magit-remote-configure "magit-remote" nil t)

(register-definition-prefixes "magit-remote" '("magit-"))

;;;***

;;;### (autoloads nil "magit-repos" "magit-repos.el" (0 0 0 0))
;;; Generated autoloads from magit-repos.el

(autoload 'magit-list-repositories "magit-repos" "\
Display a list of repositories.

Use the options `magit-repository-directories' to control which
repositories are displayed." t nil)

(register-definition-prefixes "magit-repos" '("magit-"))

;;;***

;;;### (autoloads nil "magit-reset" "magit-reset.el" (0 0 0 0))
;;; Generated autoloads from magit-reset.el
 (autoload 'magit-reset "magit" nil t)

(autoload 'magit-reset-mixed "magit-reset" "\
Reset the `HEAD' and index to COMMIT, but not the working tree.

\(git reset --mixed COMMIT)

\(fn COMMIT)" t nil)

(autoload 'magit-reset-soft "magit-reset" "\
Reset the `HEAD' to COMMIT, but not the index and working tree.

\(git reset --soft REVISION)

\(fn COMMIT)" t nil)

(autoload 'magit-reset-hard "magit-reset" "\
Reset the `HEAD', index, and working tree to COMMIT.

\(git reset --hard REVISION)

\(fn COMMIT)" t nil)

(autoload 'magit-reset-keep "magit-reset" "\
Reset the `HEAD' and index to COMMIT, while keeping uncommitted changes.

\(git reset --keep REVISION)

\(fn COMMIT)" t nil)

(autoload 'magit-reset-index "magit-reset" "\
Reset the index to COMMIT.
Keep the `HEAD' and working tree as-is, so if COMMIT refers to the
head this effectively unstages all changes.

\(git reset COMMIT .)

\(fn COMMIT)" t nil)

(autoload 'magit-reset-worktree "magit-reset" "\
Reset the worktree to COMMIT.
Keep the `HEAD' and index as-is.

\(fn COMMIT)" t nil)

(autoload 'magit-reset-quickly "magit-reset" "\
Reset the `HEAD' and index to COMMIT, and possibly the working tree.
With a prefix argument reset the working tree otherwise don't.

\(git reset --mixed|--hard COMMIT)

\(fn COMMIT &optional HARD)" t nil)

(register-definition-prefixes "magit-reset" '("magit-reset-"))

;;;***

;;;### (autoloads nil "magit-section" "magit-section.el" (0 0 0 0))
;;; Generated autoloads from magit-section.el

(register-definition-prefixes "magit-section" '("isearch-clean-overlays@magit-mode" "magit-"))

;;;***

;;;### (autoloads nil "magit-sequence" "magit-sequence.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from magit-sequence.el

(autoload 'magit-sequencer-continue "magit-sequence" "\
Resume the current cherry-pick or revert sequence." t nil)

(autoload 'magit-sequencer-skip "magit-sequence" "\
Skip the stopped at commit during a cherry-pick or revert sequence." t nil)

(autoload 'magit-sequencer-abort "magit-sequence" "\
Abort the current cherry-pick or revert sequence.
This discards all changes made since the sequence started." t nil)
 (autoload 'magit-cherry-pick "magit-sequence" nil t)

(autoload 'magit-cherry-copy "magit-sequence" "\
Copy COMMITS from another branch onto the current branch.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting.

\(fn COMMITS &optional ARGS)" t nil)

(autoload 'magit-cherry-apply "magit-sequence" "\
Apply the changes in COMMITS but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting.

\(fn COMMITS &optional ARGS)" t nil)

(autoload 'magit-cherry-harvest "magit-sequence" "\
Move COMMITS from another BRANCH onto the current branch.
Remove the COMMITS from BRANCH and stay on the current branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

\(fn COMMITS BRANCH &optional ARGS)" t nil)

(autoload 'magit-cherry-donate "magit-sequence" "\
Move COMMITS from the current branch onto another existing BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

\(fn COMMITS BRANCH &optional ARGS)" t nil)

(autoload 'magit-cherry-spinout "magit-sequence" "\
Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

\(fn COMMITS BRANCH START-POINT &optional ARGS)" t nil)

(autoload 'magit-cherry-spinoff "magit-sequence" "\
Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and checkout BRANCH.
If a conflict occurs, then you have to fix that and finish
the process manually.

\(fn COMMITS BRANCH START-POINT &optional ARGS)" t nil)
 (autoload 'magit-revert "magit-sequence" nil t)

(autoload 'magit-revert-and-commit "magit-sequence" "\
Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-revert-no-commit "magit-sequence" "\
Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

\(fn COMMIT &optional ARGS)" t nil)
 (autoload 'magit-am "magit-sequence" nil t)

(autoload 'magit-am-apply-patches "magit-sequence" "\
Apply the patches FILES.

\(fn &optional FILES ARGS)" t nil)

(autoload 'magit-am-apply-maildir "magit-sequence" "\
Apply the patches from MAILDIR.

\(fn &optional MAILDIR ARGS)" t nil)

(autoload 'magit-am-continue "magit-sequence" "\
Resume the current patch applying sequence." t nil)

(autoload 'magit-am-skip "magit-sequence" "\
Skip the stopped at patch during a patch applying sequence." t nil)

(autoload 'magit-am-abort "magit-sequence" "\
Abort the current patch applying sequence.
This discards all changes made since the sequence started." t nil)
 (autoload 'magit-rebase "magit-sequence" nil t)
 (autoload 'magit-rebase-onto-pushremote "magit-sequence" nil t)
 (autoload 'magit-rebase-onto-upstream "magit-sequence" nil t)

(autoload 'magit-rebase-branch "magit-sequence" "\
Rebase the current branch onto a branch read in the minibuffer.
All commits that are reachable from `HEAD' but not from the
selected branch TARGET are being rebased.

\(fn TARGET ARGS)" t nil)

(autoload 'magit-rebase-subset "magit-sequence" "\
Rebase a subset of the current branch's history onto a new base.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits.

\(fn NEWBASE START ARGS)" t nil)

(autoload 'magit-rebase-interactive "magit-sequence" "\
Start an interactive rebase sequence.

\(fn COMMIT ARGS)" t nil)

(autoload 'magit-rebase-autosquash "magit-sequence" "\
Combine squash and fixup commits with their intended targets.

\(fn ARGS)" t nil)

(autoload 'magit-rebase-edit-commit "magit-sequence" "\
Edit a single older commit using rebase.

\(fn COMMIT ARGS)" t nil)

(autoload 'magit-rebase-reword-commit "magit-sequence" "\
Reword a single older commit using rebase.

\(fn COMMIT ARGS)" t nil)

(autoload 'magit-rebase-remove-commit "magit-sequence" "\
Remove a single older commit using rebase.

\(fn COMMIT ARGS)" t nil)

(autoload 'magit-rebase-continue "magit-sequence" "\
Restart the current rebasing operation.
In some cases this pops up a commit message buffer for you do
edit.  With a prefix argument the old message is reused as-is.

\(fn &optional NOEDIT)" t nil)

(autoload 'magit-rebase-skip "magit-sequence" "\
Skip the current commit and restart the current rebase operation." t nil)

(autoload 'magit-rebase-edit "magit-sequence" "\
Edit the todo list of the current rebase operation." t nil)

(autoload 'magit-rebase-abort "magit-sequence" "\
Abort the current rebase operation, restoring the original branch." t nil)

(register-definition-prefixes "magit-sequence" '("magit-"))

;;;***

;;;### (autoloads nil "magit-stash" "magit-stash.el" (0 0 0 0))
;;; Generated autoloads from magit-stash.el
 (autoload 'magit-stash "magit-stash" nil t)

(autoload 'magit-stash-both "magit-stash" "\
Create a stash of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-index "magit-stash" "\
Create a stash of the index only.
Unstaged and untracked changes are not stashed.  The stashed
changes are applied in reverse to both the index and the
worktree.  This command can fail when the worktree is not clean.
Applying the resulting stash has the inverse effect.

\(fn MESSAGE)" t nil)

(autoload 'magit-stash-worktree "magit-stash" "\
Create a stash of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-keep-index "magit-stash" "\
Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-snapshot-both "magit-stash" "\
Create a snapshot of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-snapshot-index "magit-stash" "\
Create a snapshot of the index only.
Unstaged and untracked changes are not stashed." t nil)

(autoload 'magit-snapshot-worktree "magit-stash" "\
Create a snapshot of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-apply "magit-stash" "\
Apply a stash to the working tree.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index.

\(fn STASH)" t nil)

(autoload 'magit-stash-drop "magit-stash" "\
Remove a stash from the stash list.
When the region is active offer to drop all contained stashes.

\(fn STASH)" t nil)

(autoload 'magit-stash-clear "magit-stash" "\
Remove all stashes saved in REF's reflog by deleting REF.

\(fn REF)" t nil)

(autoload 'magit-stash-branch "magit-stash" "\
Create and checkout a new BRANCH from STASH.

\(fn STASH BRANCH)" t nil)

(autoload 'magit-stash-branch-here "magit-stash" "\
Create and checkout a new BRANCH and apply STASH.
The branch is created using `magit-branch-and-checkout', using the
current branch or `HEAD' as the start-point.

\(fn STASH BRANCH)" t nil)

(autoload 'magit-stash-format-patch "magit-stash" "\
Create a patch from STASH

\(fn STASH)" t nil)

(autoload 'magit-stash-list "magit-stash" "\
List all stashes in a buffer." t nil)

(autoload 'magit-stash-show "magit-stash" "\
Show all diffs of a stash in a buffer.

\(fn STASH &optional ARGS FILES)" t nil)

(register-definition-prefixes "magit-stash" '("magit-"))

;;;***

;;;### (autoloads nil "magit-status" "magit-status.el" (0 0 0 0))
;;; Generated autoloads from magit-status.el

(autoload 'magit-init "magit-status" "\
Initialize a Git repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally.

\(fn DIRECTORY)" t nil)

(autoload 'magit-status "magit-status" "\
Show the status of the current Git repository in a buffer.

If the current directory isn't located within a Git repository,
then prompt for an existing repository or an arbitrary directory,
depending on option `magit-repository-directories', and show the
status of the selected repository instead.

* If that option specifies any existing repositories, then offer
  those for completion and show the status buffer for the
  selected one.

* Otherwise read an arbitrary directory using regular file-name
  completion.  If the selected directory is the top-level of an
  existing working tree, then show the status buffer for that.

* Otherwise offer to initialize the selected directory as a new
  repository.  After creating the repository show its status
  buffer.

These fallback behaviors can also be forced using one or more
prefix arguments:

* With two prefix arguments (or more precisely a numeric prefix
  value of 16 or greater) read an arbitrary directory and act on
  it as described above.  The same could be accomplished using
  the command `magit-init'.

* With a single prefix argument read an existing repository, or
  if none can be found based on `magit-repository-directories',
  then fall back to the same behavior as with two prefix
  arguments.

\(fn &optional DIRECTORY CACHE)" t nil)

(defalias 'magit 'magit-status "\
An alias for `magit-status' for better discoverability.

Instead of invoking this alias for `magit-status' using
\"M-x magit RET\", you should bind a key to `magit-status'
and read the info node `(magit)Getting Started', which
also contains other useful hints.")

(autoload 'magit-status-here "magit-status" "\
Like `magit-status' but with non-nil `magit-status-goto-file-position'." t nil)

(autoload 'magit-status-setup-buffer "magit-status" "\


\(fn &optional DIRECTORY)" nil nil)

(register-definition-prefixes "magit-status" '("magit-"))

;;;***

;;;### (autoloads nil "magit-submodule" "magit-submodule.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from magit-submodule.el
 (autoload 'magit-submodule "magit-submodule" nil t)
 (autoload 'magit-submodule-add "magit-submodule" nil t)

(autoload 'magit-submodule-read-name-for-path "magit-submodule" "\


\(fn PATH &optional PREFER-SHORT)" nil nil)
 (autoload 'magit-submodule-register "magit-submodule" nil t)
 (autoload 'magit-submodule-populate "magit-submodule" nil t)
 (autoload 'magit-submodule-update "magit-submodule" nil t)
 (autoload 'magit-submodule-synchronize "magit-submodule" nil t)
 (autoload 'magit-submodule-unpopulate "magit-submodule" nil t)

(autoload 'magit-submodule-remove "magit-submodule" "\
Unregister MODULES and remove their working directories.

For safety reasons, do not remove the gitdirs and if a module has
uncommitted changes, then do not remove it at all.  If a module's
gitdir is located inside the working directory, then move it into
the gitdir of the superproject first.

With the \"--force\" argument offer to remove dirty working
directories and with a prefix argument offer to delete gitdirs.
Both actions are very dangerous and have to be confirmed.  There
are additional safety precautions in place, so you might be able
to recover from making a mistake here, but don't count on it.

\(fn MODULES ARGS TRASH-GITDIRS)" t nil)

(autoload 'magit-insert-modules "magit-submodule" "\
Insert submodule sections.
Hook `magit-module-sections-hook' controls which module sections
are inserted, and option `magit-module-sections-nested' controls
whether they are wrapped in an additional section." nil nil)

(autoload 'magit-insert-modules-overview "magit-submodule" "\
Insert sections for all modules.
For each section insert the path and the output of `git describe --tags',
or, failing that, the abbreviated HEAD commit hash." nil nil)

(autoload 'magit-insert-modules-unpulled-from-upstream "magit-submodule" "\
Insert sections for modules that haven't been pulled from the upstream.
These sections can be expanded to show the respective commits." nil nil)

(autoload 'magit-insert-modules-unpulled-from-pushremote "magit-submodule" "\
Insert sections for modules that haven't been pulled from the push-remote.
These sections can be expanded to show the respective commits." nil nil)

(autoload 'magit-insert-modules-unpushed-to-upstream "magit-submodule" "\
Insert sections for modules that haven't been pushed to the upstream.
These sections can be expanded to show the respective commits." nil nil)

(autoload 'magit-insert-modules-unpushed-to-pushremote "magit-submodule" "\
Insert sections for modules that haven't been pushed to the push-remote.
These sections can be expanded to show the respective commits." nil nil)

(autoload 'magit-list-submodules "magit-submodule" "\
Display a list of the current repository's submodules." t nil)

(register-definition-prefixes "magit-submodule" '("magit-"))

;;;***

;;;### (autoloads nil "magit-subtree" "magit-subtree.el" (0 0 0 0))
;;; Generated autoloads from magit-subtree.el
 (autoload 'magit-subtree "magit-subtree" nil t)
 (autoload 'magit-subtree-import "magit-subtree" nil t)
 (autoload 'magit-subtree-export "magit-subtree" nil t)

(autoload 'magit-subtree-add "magit-subtree" "\
Add REF from REPOSITORY as a new subtree at PREFIX.

\(fn PREFIX REPOSITORY REF ARGS)" t nil)

(autoload 'magit-subtree-add-commit "magit-subtree" "\
Add COMMIT as a new subtree at PREFIX.

\(fn PREFIX COMMIT ARGS)" t nil)

(autoload 'magit-subtree-merge "magit-subtree" "\
Merge COMMIT into the PREFIX subtree.

\(fn PREFIX COMMIT ARGS)" t nil)

(autoload 'magit-subtree-pull "magit-subtree" "\
Pull REF from REPOSITORY into the PREFIX subtree.

\(fn PREFIX REPOSITORY REF ARGS)" t nil)

(autoload 'magit-subtree-push "magit-subtree" "\
Extract the history of the subtree PREFIX and push it to REF on REPOSITORY.

\(fn PREFIX REPOSITORY REF ARGS)" t nil)

(autoload 'magit-subtree-split "magit-subtree" "\
Extract the history of the subtree PREFIX.

\(fn PREFIX COMMIT ARGS)" t nil)

(register-definition-prefixes "magit-subtree" '("magit-"))

;;;***

;;;### (autoloads nil "magit-tag" "magit-tag.el" (0 0 0 0))
;;; Generated autoloads from magit-tag.el
 (autoload 'magit-tag "magit" nil t)

(autoload 'magit-tag-create "magit-tag" "\
Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.

\(git tag [--annotate] NAME REV)

\(fn NAME REV &optional ARGS)" t nil)

(autoload 'magit-tag-delete "magit-tag" "\
Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.

\(git tag -d TAGS)

\(fn TAGS)" t nil)

(autoload 'magit-tag-prune "magit-tag" "\
Offer to delete tags missing locally from REMOTE, and vice versa.

\(fn TAGS REMOTE-TAGS REMOTE)" t nil)

(autoload 'magit-tag-release "magit-tag" "\
Create a release tag.

Assume that release tags match `magit-release-tag-regexp'.

First prompt for the name of the new tag using the highest
existing tag as initial input and leaving it to the user to
increment the desired part of the version string.

If `--annotate' is enabled, then prompt for the message of the
new tag.  Base the proposed tag message on the message of the
highest tag, provided that that contains the corresponding
version string and substituting the new version string for that.
Otherwise propose something like \"Foo-Bar 1.2.3\", given, for
example, a TAG \"v1.2.3\" and a repository located at something
like \"/path/to/foo-bar\".

\(fn TAG MSG &optional ARGS)" t nil)

(register-definition-prefixes "magit-tag" '("magit-"))

;;;***

;;;### (autoloads nil "magit-transient" "magit-transient.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from magit-transient.el

(register-definition-prefixes "magit-transient" '("magit-"))

;;;***

;;;### (autoloads nil "magit-utils" "magit-utils.el" (0 0 0 0))
;;; Generated autoloads from magit-utils.el

(autoload 'magit-emacs-Q-command "magit-utils" "\
Show a shell command that runs an uncustomized Emacs with only Magit loaded.
See info node `(magit)Debugging Tools' for more information." t nil)

(autoload 'Info-follow-nearest-node--magit-gitman "magit-utils" "\


\(fn FN &optional FORK)" nil nil)

(advice-add 'Info-follow-nearest-node :around 'Info-follow-nearest-node--magit-gitman)

(autoload 'org-man-export--magit-gitman "magit-utils" "\


\(fn FN LINK DESCRIPTION FORMAT)" nil nil)

(advice-add 'org-man-export :around 'org-man-export--magit-gitman)

(register-definition-prefixes "magit-utils" '("magit-"))

;;;***

;;;### (autoloads nil "magit-wip" "magit-wip.el" (0 0 0 0))
;;; Generated autoloads from magit-wip.el

(defvar magit-wip-mode nil "\
Non-nil if Magit-Wip mode is enabled.
See the `magit-wip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-mode'.")

(custom-autoload 'magit-wip-mode "magit-wip" nil)

(autoload 'magit-wip-mode "magit-wip" "\
Save uncommitted changes to work-in-progress refs.

If called interactively, toggle `Magit-Wip mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Whenever appropriate (i.e. when dataloss would be a possibility
otherwise) this mode causes uncommitted changes to be committed
to dedicated work-in-progress refs.

For historic reasons this mode is implemented on top of four
other `magit-wip-*' modes, which can also be used individually,
if you want finer control over when the wip refs are updated;
but that is discouraged.

\(fn &optional ARG)" t nil)

(put 'magit-wip-after-save-mode 'globalized-minor-mode t)

(defvar magit-wip-after-save-mode nil "\
Non-nil if Magit-Wip-After-Save mode is enabled.
See the `magit-wip-after-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-after-save-mode'.")

(custom-autoload 'magit-wip-after-save-mode "magit-wip" nil)

(autoload 'magit-wip-after-save-mode "magit-wip" "\
Toggle Magit-Wip-After-Save-Local mode in all buffers.
With prefix ARG, enable Magit-Wip-After-Save mode if ARG is
positive; otherwise, disable it.  If called from Lisp, enable the mode if ARG
is omitted or nil.

Magit-Wip-After-Save-Local mode is enabled in all buffers where
`magit-wip-after-save-local-mode-turn-on' would do it.

See `magit-wip-after-save-local-mode' for more information on
Magit-Wip-After-Save-Local mode.

\(fn &optional ARG)" t nil)

(defvar magit-wip-after-apply-mode nil "\
Non-nil if Magit-Wip-After-Apply mode is enabled.
See the `magit-wip-after-apply-mode' command
for a description of this minor mode.")

(custom-autoload 'magit-wip-after-apply-mode "magit-wip" nil)

(autoload 'magit-wip-after-apply-mode "magit-wip" "\
Commit to work-in-progress refs.

If called interactively, toggle `Magit-Wip-After-Apply mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

After applying a change using any \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected files to the current wip refs.  For each branch there
may be two wip refs; one contains snapshots of the files as found
in the worktree and the other contains snapshots of the entries
in the index.

\(fn &optional ARG)" t nil)

(defvar magit-wip-before-change-mode nil "\
Non-nil if Magit-Wip-Before-Change mode is enabled.
See the `magit-wip-before-change-mode' command
for a description of this minor mode.")

(custom-autoload 'magit-wip-before-change-mode "magit-wip" nil)

(autoload 'magit-wip-before-change-mode "magit-wip" "\
Commit to work-in-progress refs before certain destructive changes.

If called interactively, toggle `Magit-Wip-Before-Change mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Before invoking a revert command or an \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected tracked files to the current wip refs.  For each branch
there may be two wip refs; one contains snapshots of the files
as found in the worktree and the other contains snapshots of the
entries in the index.

Only changes to files which could potentially be affected by the
command which is about to be called are committed.

\(fn &optional ARG)" t nil)

(autoload 'magit-wip-commit-initial-backup "magit-wip" "\
Before saving, commit current file to a worktree wip ref.

The user has to add this function to `before-save-hook'.

Commit the current state of the visited file before saving the
current buffer to that file.  This backs up the same version of
the file as `backup-buffer' would, but stores the backup in the
worktree wip ref, which is also used by the various Magit Wip
modes, instead of in a backup file as `backup-buffer' would.

This function ignores the variables that affect `backup-buffer'
and can be used along-side that function, which is recommended
because this function only backs up files that are tracked in
a Git repository." nil nil)

(register-definition-prefixes "magit-wip" '("magit-"))

;;;***

;;;### (autoloads nil "magit-worktree" "magit-worktree.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from magit-worktree.el
 (autoload 'magit-worktree "magit-worktree" nil t)

(autoload 'magit-worktree-checkout "magit-worktree" "\
Checkout BRANCH in a new worktree at PATH.

\(fn PATH BRANCH)" t nil)

(autoload 'magit-worktree-branch "magit-worktree" "\
Create a new BRANCH and check it out in a new worktree at PATH.

\(fn PATH BRANCH START-POINT &optional FORCE)" t nil)

(autoload 'magit-worktree-move "magit-worktree" "\
Move WORKTREE to PATH.

\(fn WORKTREE PATH)" t nil)

(register-definition-prefixes "magit-worktree" '("magit-"))

;;;***

;;;### (autoloads nil nil ("magit-core.el" "magit-pkg.el") (0 0 0
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-autoloads.el ends here
;;; markdown-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "markdown-mode" "markdown-mode.el" (0 0 0 0))
;;; Generated autoloads from markdown-mode.el

(autoload 'markdown-mode "markdown-mode" "\
Major mode for editing Markdown files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode" "\
Major mode for editing GitHub Flavored Markdown files.

\(fn)" t nil)

(autoload 'markdown-view-mode "markdown-mode" "\
Major mode for viewing Markdown content.

\(fn)" t nil)

(autoload 'gfm-view-mode "markdown-mode" "\
Major mode for viewing GitHub Flavored Markdown content.

\(fn)" t nil)

(autoload 'markdown-live-preview-mode "markdown-mode" "\
Toggle native previewing on save for a specific markdown file.

If called interactively, toggle `Markdown-Live-Preview mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "markdown-mode" '("defun-markdown-" "gfm-" "markdown"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; markdown-mode-autoloads.el ends here
;;; markdown-toc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "markdown-toc" "markdown-toc.el" (0 0 0 0))
;;; Generated autoloads from markdown-toc.el

(autoload 'markdown-toc-version "markdown-toc" "\
Markdown-toc version." t nil)

(autoload 'markdown-toc-generate-toc "markdown-toc" "\
Generate a TOC for markdown file at current point.
Deletes any previous TOC.
If called interactively with prefix arg REPLACE-TOC-P, replaces previous TOC.

\(fn &optional REPLACE-TOC-P)" t nil)

(autoload 'markdown-toc-generate-or-refresh-toc "markdown-toc" "\
Generate a TOC for markdown file at current point or refreshes an already generated TOC." t nil)

(autoload 'markdown-toc-refresh-toc "markdown-toc" "\
Refreshes an already generated TOC." t nil)

(autoload 'markdown-toc-delete-toc "markdown-toc" "\
Deletes a previously generated TOC." t nil)

(autoload 'markdown-toc-follow-link-at-point "markdown-toc" "\
On a given toc link, navigate to the current markdown header.
If the toc is misindented (according to markdown-toc-indentation-space`)
or if not on a toc link, this does nothing.
" t nil)

(autoload 'markdown-toc-mode "markdown-toc" "\
Functionality for generating toc in markdown file.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

If called interactively, enable Markdown-Toc mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Commands:
\\{markdown-toc-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "markdown-toc" '("markdown-")))

;;;***

;;;### (autoloads nil nil ("markdown-toc-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; markdown-toc-autoloads.el ends here
;;; mmm-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mmm-auto" "mmm-auto.el" (0 0 0 0))
;;; Generated autoloads from mmm-auto.el

(register-definition-prefixes "mmm-auto" '("mmm-"))

;;;***

;;;### (autoloads nil "mmm-class" "mmm-class.el" (0 0 0 0))
;;; Generated autoloads from mmm-class.el

(register-definition-prefixes "mmm-class" '("mmm-"))

;;;***

;;;### (autoloads nil "mmm-cmds" "mmm-cmds.el" (0 0 0 0))
;;; Generated autoloads from mmm-cmds.el

(register-definition-prefixes "mmm-cmds" '("mmm-"))

;;;***

;;;### (autoloads nil "mmm-compat" "mmm-compat.el" (0 0 0 0))
;;; Generated autoloads from mmm-compat.el

(register-definition-prefixes "mmm-compat" '("mmm-" "skeleton-positions"))

;;;***

;;;### (autoloads nil "mmm-cweb" "mmm-cweb.el" (0 0 0 0))
;;; Generated autoloads from mmm-cweb.el

(register-definition-prefixes "mmm-cweb" '("mmm-cweb-"))

;;;***

;;;### (autoloads nil "mmm-erb" "mmm-erb.el" (0 0 0 0))
;;; Generated autoloads from mmm-erb.el

(autoload 'html-erb-mode "mmm-erb" "\


\(fn)" t nil)

(autoload 'nxml-web-mode "mmm-erb" "\


\(fn)" t nil)

(register-definition-prefixes "mmm-erb" '("html-erb-after-syntax-propertize" "mmm-erb-"))

;;;***

;;;### (autoloads nil "mmm-mason" "mmm-mason.el" (0 0 0 0))
;;; Generated autoloads from mmm-mason.el

(register-definition-prefixes "mmm-mason" '("mmm-mason-"))

;;;***

;;;### (autoloads nil "mmm-mode" "mmm-mode.el" (0 0 0 0))
;;; Generated autoloads from mmm-mode.el

(register-definition-prefixes "mmm-mode" '("mmm-"))

;;;***

;;;### (autoloads nil "mmm-myghty" "mmm-myghty.el" (0 0 0 0))
;;; Generated autoloads from mmm-myghty.el

(register-definition-prefixes "mmm-myghty" '("mmm-myghty-"))

;;;***

;;;### (autoloads nil "mmm-noweb" "mmm-noweb.el" (0 0 0 0))
;;; Generated autoloads from mmm-noweb.el

(register-definition-prefixes "mmm-noweb" '("mmm-"))

;;;***

;;;### (autoloads nil "mmm-region" "mmm-region.el" (0 0 0 0))
;;; Generated autoloads from mmm-region.el

(register-definition-prefixes "mmm-region" '("mmm-"))

;;;***

;;;### (autoloads nil "mmm-rpm" "mmm-rpm.el" (0 0 0 0))
;;; Generated autoloads from mmm-rpm.el

(register-definition-prefixes "mmm-rpm" '("mmm-rpm-sh-"))

;;;***

;;;### (autoloads nil "mmm-sample" "mmm-sample.el" (0 0 0 0))
;;; Generated autoloads from mmm-sample.el

(register-definition-prefixes "mmm-sample" '("mmm-"))

;;;***

;;;### (autoloads nil "mmm-univ" "mmm-univ.el" (0 0 0 0))
;;; Generated autoloads from mmm-univ.el

(register-definition-prefixes "mmm-univ" '("mmm-univ-get-mode"))

;;;***

;;;### (autoloads nil "mmm-utils" "mmm-utils.el" (0 0 0 0))
;;; Generated autoloads from mmm-utils.el

(register-definition-prefixes "mmm-utils" '("mmm-"))

;;;***

;;;### (autoloads nil "mmm-vars" "mmm-vars.el" (0 0 0 0))
;;; Generated autoloads from mmm-vars.el

(autoload 'mmm-add-classes "mmm-vars" "\
Add the submode classes CLASSES to `mmm-classes-alist'.

\(fn CLASSES)" nil nil)

(register-definition-prefixes "mmm-vars" '("mmm-"))

;;;***

;;;### (autoloads nil nil ("mmm-defaults.el" "mmm-mode-pkg.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mmm-mode-autoloads.el ends here
;;; modern-cpp-font-lock-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "modern-cpp-font-lock" "modern-cpp-font-lock.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from modern-cpp-font-lock.el

(autoload 'modern-c++-font-lock-mode "modern-cpp-font-lock" "\
Provides font-locking as a Minor Mode for Modern C++

If called interactively, toggle `Modern-C++-Font-Lock mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'modern-c++-font-lock-global-mode 'globalized-minor-mode t)

(defvar modern-c++-font-lock-global-mode nil "\
Non-nil if Modern-C++-Font-Lock-Global mode is enabled.
See the `modern-c++-font-lock-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `modern-c++-font-lock-global-mode'.")

(custom-autoload 'modern-c++-font-lock-global-mode "modern-cpp-font-lock" nil)

(autoload 'modern-c++-font-lock-global-mode "modern-cpp-font-lock" "\
Toggle Modern-C++-Font-Lock mode in all buffers.
With prefix ARG, enable Modern-C++-Font-Lock-Global mode if ARG is
positive; otherwise, disable it.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Modern-C++-Font-Lock mode is enabled in all buffers where `(lambda
nil (when (apply 'derived-mode-p '(c++-mode))
\(modern-c++-font-lock-mode 1)))' would do it.

See `modern-c++-font-lock-mode' for more information on
Modern-C++-Font-Lock mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "modern-cpp-font-lock" '("modern-c++-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; modern-cpp-font-lock-autoloads.el ends here
;;; modus-themes-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "modus-operandi-theme" "modus-operandi-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from modus-operandi-theme.el

(register-definition-prefixes "modus-operandi-theme" '("modus-operandi"))

;;;***

;;;### (autoloads nil "modus-themes" "modus-themes.el" (0 0 0 0))
;;; Generated autoloads from modus-themes.el

(autoload 'modus-themes-contrast "modus-themes" "\
Measure WCAG contrast ratio between C1 and C2.
C1 and C2 are color values written in hexadecimal RGB.

\(fn C1 C2)" nil nil)

(autoload 'modus-themes-color "modus-themes" "\
Return color value for COLOR from current palette.
COLOR is a key in `modus-themes-operandi-colors' or
`modus-themes-vivendi-colors'.

\(fn COLOR)" nil nil)

(autoload 'modus-themes-color-alts "modus-themes" "\
Return color value from current palette.
When Modus Operandi is enabled, return color value for color
LIGHT-COLOR.  When Modus Vivendi is enabled, return color value
for DARK-COLOR.  LIGHT-COLOR and DARK-COLOR are keys in
`modus-themes-operandi-colors' or `modus-themes-vivendi-colors'.

\(fn LIGHT-COLOR DARK-COLOR)" nil nil)

(autoload 'modus-themes-load-themes "modus-themes" "\
Ensure that the Modus themes are in `custom-enabled-themes'.

This function is intended for use in package declarations such as
those defined with the help of `use-package'.  The idea is to add
this function to the `:init' stage of the package's loading, so
that subsequent calls that assume the presence of a loaded theme,
like `modus-themes-toggle' or `modus-themes-load-operandi', will
continue to work as intended even if they are lazy-loaded (such
as when they are declared in the `:config' phase)." nil nil)

(autoload 'modus-themes-load-operandi "modus-themes" "\
Load `modus-operandi' and disable `modus-vivendi'.
Also run `modus-themes-after-load-theme-hook'." nil nil)

(autoload 'modus-themes-load-vivendi "modus-themes" "\
Load `modus-vivendi' and disable `modus-operandi'.
Also run `modus-themes-after-load-theme-hook'." nil nil)

(autoload 'modus-themes-toggle "modus-themes" "\
Toggle between `modus-operandi' and `modus-vivendi' themes.
Also runs `modus-themes-after-load-theme-hook' at its last stage
by virtue of calling either of `modus-themes-load-operandi' and
`modus-themes-load-vivendi' functions." t nil)

(when load-file-name (let ((dir (file-name-directory load-file-name))) (unless (equal dir (expand-file-name "themes/" data-directory)) (add-to-list 'custom-theme-load-path dir))))

(register-definition-prefixes "modus-themes" '("modus-themes-"))

;;;***

;;;### (autoloads nil "modus-vivendi-theme" "modus-vivendi-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from modus-vivendi-theme.el

(register-definition-prefixes "modus-vivendi-theme" '("modus-vivendi"))

;;;***

;;;### (autoloads nil nil ("modus-themes-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; modus-themes-autoloads.el ends here
;;; ob-go-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ob-go" "ob-go.el" (0 0 0 0))
;;; Generated autoloads from ob-go.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-go" '("org-babel-")))

;;;***

;;;### (autoloads nil "test-ob-go" "test-ob-go.el" (0 0 0 0))
;;; Generated autoloads from test-ob-go.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "test-ob-go" '("ob-go-test-" "org-")))

;;;***

;;;### (autoloads nil nil ("ob-go-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ob-go-autoloads.el ends here
;;; ob-rust-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ob-rust" "ob-rust.el" (0 0 0 0))
;;; Generated autoloads from ob-rust.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-rust" '("org-babel-")))

;;;***

;;;### (autoloads nil "test-ob-rust" "test-ob-rust.el" (0 0 0 0))
;;; Generated autoloads from test-ob-rust.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "test-ob-rust" '("ob-rust-test-" "org-")))

;;;***

;;;### (autoloads nil nil ("ob-rust-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ob-rust-autoloads.el ends here
;;; org-re-reveal-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-re-reveal" "org-re-reveal.el" (0 0 0 0))
;;; Generated autoloads from org-re-reveal.el

(autoload 'org-re-reveal-publish-to-reveal "org-re-reveal" "\
Publish an Org file to HTML.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.  Optional BACKEND may specify a derived export
backend.
Return output file name.

\(fn PLIST FILENAME PUB-DIR &optional BACKEND)" nil nil)

(autoload 'org-re-reveal-publish-to-reveal-client "org-re-reveal" "\
Publish an Org file to HTML as multiplex client.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.  Optional BACKEND may specify a derived export
backend.
If `org-re-reveal-client-multiplex-filter' is non-nil, use it as regular
expression to only publish FILENAME if it matches this regular expression.
Return output file name.

\(fn PLIST FILENAME PUB-DIR &optional BACKEND)" nil nil)

(autoload 'org-re-reveal-version "org-re-reveal" "\
Display version string for org-re-reveal from Lisp file." t nil)

(register-definition-prefixes "org-re-reveal" '("org-re-reveal-"))

;;;***

;;;### (autoloads nil nil ("org-re-reveal-pkg.el" "ox-re-reveal.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-re-reveal-autoloads.el ends here
;;; page-break-lines-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "page-break-lines" "page-break-lines.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from page-break-lines.el

(autoload 'page-break-lines-mode "page-break-lines" "\
Toggle Page Break Lines mode.

If called interactively, enable Page-Break-Lines mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

In Page Break mode, page breaks (^L characters) are displayed as a
horizontal line of `page-break-lines-char' characters.

\(fn &optional ARG)" t nil)

(autoload 'page-break-lines-mode-maybe "page-break-lines" "\
Enable `page-break-lines-mode' in the current buffer if desired.
When `major-mode' is listed in `page-break-lines-modes', then
`page-break-lines-mode' will be enabled." nil nil)

(put 'global-page-break-lines-mode 'globalized-minor-mode t)

(defvar global-page-break-lines-mode nil "\
Non-nil if Global Page-Break-Lines mode is enabled.
See the `global-page-break-lines-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-page-break-lines-mode'.")

(custom-autoload 'global-page-break-lines-mode "page-break-lines" nil)

(autoload 'global-page-break-lines-mode "page-break-lines" "\
Toggle Page-Break-Lines mode in all buffers.
With prefix ARG, enable Global Page-Break-Lines mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Page-Break-Lines mode is enabled in all buffers where
`page-break-lines-mode-maybe' would do it.
See `page-break-lines-mode' for more information on Page-Break-Lines mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "page-break-lines" '("page-break-lines-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; page-break-lines-autoloads.el ends here
;;; paredit-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "paredit" "paredit.el" (0 0 0 0))
;;; Generated autoloads from paredit.el

(autoload 'paredit-mode "paredit" "\
Minor mode for pseudo-structurally editing Lisp code.
With a prefix argument, enable Paredit Mode even if there are
  unbalanced parentheses in the buffer.
Paredit behaves badly if parentheses are unbalanced, so exercise
  caution when forcing Paredit Mode to be enabled, and consider
  fixing unbalanced parentheses instead.
\\<paredit-mode-map>

If called interactively, toggle `Paredit mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'enable-paredit-mode "paredit" "\
Turn on pseudo-structural editing of Lisp code." t nil)

(register-definition-prefixes "paredit" '("?\\" "disable-paredit-mode" "paredit-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; paredit-autoloads.el ends here
;;; parseclj-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "parseclj" "parseclj.el" (0 0 0 0))
;;; Generated autoloads from parseclj.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "parseclj" '("parseclj-")))

;;;***

;;;### (autoloads nil "parseclj-ast" "parseclj-ast.el" (0 0 0 0))
;;; Generated autoloads from parseclj-ast.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "parseclj-ast" '("parseclj-ast-")))

;;;***

;;;### (autoloads nil "parseclj-lex" "parseclj-lex.el" (0 0 0 0))
;;; Generated autoloads from parseclj-lex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "parseclj-lex" '("parseclj-lex-")))

;;;***

;;;### (autoloads nil "parseclj-parser" "parseclj-parser.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from parseclj-parser.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "parseclj-parser" '("parseclj-")))

;;;***

;;;### (autoloads nil nil ("parseclj-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; parseclj-autoloads.el ends here
;;; parseedn-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "parseedn" "parseedn.el" (0 0 0 0))
;;; Generated autoloads from parseedn.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "parseedn" '("parseedn-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; parseedn-autoloads.el ends here
;;; pdf-tools-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pdf-annot" "pdf-annot.el" (0 0 0 0))
;;; Generated autoloads from pdf-annot.el

(autoload 'pdf-annot-minor-mode "pdf-annot" "\
Support for PDF Annotations.

If called interactively, toggle `Pdf-Annot minor mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{pdf-annot-minor-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "pdf-annot" '("pdf-annot-"))

;;;***

;;;### (autoloads nil "pdf-cache" "pdf-cache.el" (0 0 0 0))
;;; Generated autoloads from pdf-cache.el

(register-definition-prefixes "pdf-cache" '("boundingbox" "define-pdf-cache-function" "page" "pdf-cache-" "textregions"))

;;;***

;;;### (autoloads nil "pdf-dev" "pdf-dev.el" (0 0 0 0))
;;; Generated autoloads from pdf-dev.el

(register-definition-prefixes "pdf-dev" '("pdf-dev-"))

;;;***

;;;### (autoloads nil "pdf-history" "pdf-history.el" (0 0 0 0))
;;; Generated autoloads from pdf-history.el

(autoload 'pdf-history-minor-mode "pdf-history" "\
Keep a history of previously visited pages.

If called interactively, toggle `Pdf-History minor mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This is a simple stack-based history.  Turning the page or
following a link pushes the left-behind page on the stack, which
may be navigated with the following keys.

\\{pdf-history-minor-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "pdf-history" '("pdf-history-"))

;;;***

;;;### (autoloads nil "pdf-info" "pdf-info.el" (0 0 0 0))
;;; Generated autoloads from pdf-info.el

(register-definition-prefixes "pdf-info" '("pdf-info-"))

;;;***

;;;### (autoloads nil "pdf-isearch" "pdf-isearch.el" (0 0 0 0))
;;; Generated autoloads from pdf-isearch.el

(autoload 'pdf-isearch-minor-mode "pdf-isearch" "\
Isearch mode for PDF buffer.

If called interactively, toggle `Pdf-Isearch minor mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When this mode is enabled \\[isearch-forward], among other keys,
starts an incremental search in this PDF document.  Since this mode
uses external programs to highlight found matches via
image-processing, proceeding to the next match may be slow.

Therefore two isearch behaviours have been defined: Normal isearch and
batch mode.  The later one is a minor mode
\(`pdf-isearch-batch-mode'), which when activated inhibits isearch
from stopping at and highlighting every single match, but rather
display them batch-wise.  Here a batch means a number of matches
currently visible in the selected window.

The kind of highlighting is determined by three faces
`pdf-isearch-match' (for the current match), `pdf-isearch-lazy'
\(for all other matches) and `pdf-isearch-batch' (when in batch
mode), which see.

Colors may also be influenced by the minor-mode
`pdf-view-dark-minor-mode'.  If this is minor mode enabled, each face's
dark colors, are used (see e.g. `frame-background-mode'), instead
of the light ones.

\\{pdf-isearch-minor-mode-map}
While in `isearch-mode' the following keys are available. Note
that not every isearch command work as expected.

\\{pdf-isearch-active-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "pdf-isearch" '("pdf-isearch-"))

;;;***

;;;### (autoloads nil "pdf-links" "pdf-links.el" (0 0 0 0))
;;; Generated autoloads from pdf-links.el

(autoload 'pdf-links-minor-mode "pdf-links" "\
Handle links in PDF documents.\\<pdf-links-minor-mode-map>

If called interactively, toggle `Pdf-Links minor mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

If this mode is enabled, most links in the document may be
activated by clicking on them or by pressing \\[pdf-links-action-perform] and selecting
one of the displayed keys, or by using isearch limited to
links via \\[pdf-links-isearch-link].

\\{pdf-links-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'pdf-links-action-perform "pdf-links" "\
Follow LINK, depending on its type.

This may turn to another page, switch to another PDF buffer or
invoke `pdf-links-browse-uri-function'.

Interactively, link is read via `pdf-links-read-link-action'.
This function displays characters around the links in the current
page and starts reading characters (ignoring case).  After a
sufficient number of characters have been read, the corresponding
link's link is invoked.  Additionally, SPC may be used to
scroll the current page.

\(fn LINK)" t nil)

(register-definition-prefixes "pdf-links" '("pdf-links-"))

;;;***

;;;### (autoloads nil "pdf-loader" "pdf-loader.el" (0 0 0 0))
;;; Generated autoloads from pdf-loader.el

(autoload 'pdf-loader-install "pdf-loader" "\
Prepare Emacs for using PDF Tools.

This function acts as a replacement for `pdf-tools-install' and
makes Emacs load and use PDF Tools as soon as a PDF file is
opened, but not sooner.

The arguments are passed verbatim to `pdf-tools-install', which
see.

\(fn &optional NO-QUERY-P SKIP-DEPENDENCIES-P NO-ERROR-P FORCE-DEPENDENCIES-P)" nil nil)

(register-definition-prefixes "pdf-loader" '("pdf-loader--"))

;;;***

;;;### (autoloads nil "pdf-misc" "pdf-misc.el" (0 0 0 0))
;;; Generated autoloads from pdf-misc.el

(autoload 'pdf-misc-minor-mode "pdf-misc" "\
FIXME:  Not documented.

If called interactively, toggle `Pdf-Misc minor mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'pdf-misc-size-indication-minor-mode "pdf-misc" "\
Provide a working size indication in the mode-line.

If called interactively, toggle `Pdf-Misc-Size-Indication minor
mode'.  If the prefix argument is positive, enable the mode, and
if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'pdf-misc-menu-bar-minor-mode "pdf-misc" "\
Display a PDF Tools menu in the menu-bar.

If called interactively, toggle `Pdf-Misc-Menu-Bar minor mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'pdf-misc-context-menu-minor-mode "pdf-misc" "\
Provide a right-click context menu in PDF buffers.

If called interactively, toggle `Pdf-Misc-Context-Menu minor
mode'.  If the prefix argument is positive, enable the mode, and
if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{pdf-misc-context-menu-minor-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "pdf-misc" '("pdf-misc-"))

;;;***

;;;### (autoloads nil "pdf-occur" "pdf-occur.el" (0 0 0 0))
;;; Generated autoloads from pdf-occur.el

(autoload 'pdf-occur "pdf-occur" "\
List lines matching STRING or PCRE.

Interactively search for a regexp. Unless a prefix arg was given,
in which case this functions performs a string search.

If `pdf-occur-prefer-string-search' is non-nil, the meaning of
the prefix-arg is inverted.

\(fn STRING &optional REGEXP-P)" t nil)

(autoload 'pdf-occur-multi-command "pdf-occur" "\
Perform `pdf-occur' on multiple buffer.

For a programmatic search of multiple documents see
`pdf-occur-search'." t nil)

(defvar pdf-occur-global-minor-mode nil "\
Non-nil if Pdf-Occur-Global minor mode is enabled.
See the `pdf-occur-global-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pdf-occur-global-minor-mode'.")

(custom-autoload 'pdf-occur-global-minor-mode "pdf-occur" nil)

(autoload 'pdf-occur-global-minor-mode "pdf-occur" "\
Enable integration of Pdf Occur with other modes.

If called interactively, toggle `Pdf-Occur-Global minor mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This global minor mode enables (or disables)
`pdf-occur-ibuffer-minor-mode' and `pdf-occur-dired-minor-mode'
in all current and future ibuffer/dired buffer.

\(fn &optional ARG)" t nil)

(autoload 'pdf-occur-ibuffer-minor-mode "pdf-occur" "\
Hack into ibuffer's do-occur binding.

If called interactively, toggle `Pdf-Occur-Ibuffer minor mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This mode remaps `ibuffer-do-occur' to
`pdf-occur-ibuffer-do-occur', which will start the PDF Tools
version of `occur', if all marked buffer's are in `pdf-view-mode'
and otherwise fallback to `ibuffer-do-occur'.

\(fn &optional ARG)" t nil)

(autoload 'pdf-occur-dired-minor-mode "pdf-occur" "\
Hack into dired's `dired-do-search' binding.

If called interactively, toggle `Pdf-Occur-Dired minor mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This mode remaps `dired-do-search' to
`pdf-occur-dired-do-search', which will start the PDF Tools
version of `occur', if all marked buffer's are in `pdf-view-mode'
and otherwise fallback to `dired-do-search'.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "pdf-occur" '("pdf-occur-"))

;;;***

;;;### (autoloads nil "pdf-outline" "pdf-outline.el" (0 0 0 0))
;;; Generated autoloads from pdf-outline.el

(autoload 'pdf-outline-minor-mode "pdf-outline" "\
Display an outline of a PDF document.

If called interactively, toggle `Pdf-Outline minor mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This provides a PDF's outline on the menu bar via imenu.
Additionally the same outline may be viewed in a designated
buffer.

\\{pdf-outline-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'pdf-outline "pdf-outline" "\
Display an PDF outline of BUFFER.

BUFFER defaults to the current buffer.  Select the outline
buffer, unless NO-SELECT-WINDOW-P is non-nil.

\(fn &optional BUFFER NO-SELECT-WINDOW-P)" t nil)

(autoload 'pdf-outline-imenu-enable "pdf-outline" "\
Enable imenu in the current PDF buffer." t nil)

(register-definition-prefixes "pdf-outline" '("pdf-outline"))

;;;***

;;;### (autoloads nil "pdf-sync" "pdf-sync.el" (0 0 0 0))
;;; Generated autoloads from pdf-sync.el

(autoload 'pdf-sync-minor-mode "pdf-sync" "\
Correlate a PDF position with the TeX file.
\\<pdf-sync-minor-mode-map>
This works via SyncTeX, which means the TeX sources need to have
been compiled with `--synctex=1'.  In AUCTeX this can be done by
setting `TeX-source-correlate-method' to 'synctex (before AUCTeX
is loaded) and enabling `TeX-source-correlate-mode'.

If called interactively, toggle `Pdf-Sync minor mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Then \\[pdf-sync-backward-search-mouse] in the PDF buffer will open the
corresponding TeX location.

If AUCTeX is your preferred tex-mode, this library arranges to
bind `pdf-sync-forward-display-pdf-key' (the default is `C-c C-g')
to `pdf-sync-forward-search' in `TeX-source-correlate-map'.  This
function displays the PDF page corresponding to the current
position in the TeX buffer.  This function only works together
with AUCTeX.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "pdf-sync" '("pdf-sync-"))

;;;***

;;;### (autoloads nil "pdf-tools" "pdf-tools.el" (0 0 0 0))
;;; Generated autoloads from pdf-tools.el

(defvar pdf-tools-handle-upgrades t "\
Whether PDF Tools should handle upgrading itself.")

(custom-autoload 'pdf-tools-handle-upgrades "pdf-tools" t)

(autoload 'pdf-tools-install "pdf-tools" "\
Install PDF-Tools in all current and future PDF buffers.

If the `pdf-info-epdfinfo-program' is not running or does not
appear to be working, attempt to rebuild it.  If this build
succeeded, continue with the activation of the package.
Otherwise fail silently, i.e. no error is signaled.

Build the program (if necessary) without asking first, if
NO-QUERY-P is non-nil.

Don't attempt to install system packages, if SKIP-DEPENDENCIES-P
is non-nil.

Do not signal an error in case the build failed, if NO-ERROR-P is
non-nil.

Attempt to install system packages (even if it is deemed
unnecessary), if FORCE-DEPENDENCIES-P is non-nil.

Note that SKIP-DEPENDENCIES-P and FORCE-DEPENDENCIES-P are
mutually exclusive.

Note further, that you can influence the installation directory
by setting `pdf-info-epdfinfo-program' to an appropriate
value (e.g. ~/bin/epdfinfo) before calling this function.

See `pdf-view-mode' and `pdf-tools-enabled-modes'.

\(fn &optional NO-QUERY-P SKIP-DEPENDENCIES-P NO-ERROR-P FORCE-DEPENDENCIES-P)" t nil)

(autoload 'pdf-tools-enable-minor-modes "pdf-tools" "\
Enable MODES in the current buffer.

MODES defaults to `pdf-tools-enabled-modes'.

\(fn &optional MODES)" t nil)

(autoload 'pdf-tools-help "pdf-tools" nil t nil)

(register-definition-prefixes "pdf-tools" '("pdf-tools-"))

;;;***

;;;### (autoloads nil "pdf-util" "pdf-util.el" (0 0 0 0))
;;; Generated autoloads from pdf-util.el

(register-definition-prefixes "pdf-util" '("display-buffer-split-below-and-attach" "pdf-util-"))

;;;***

;;;### (autoloads nil "pdf-view" "pdf-view.el" (0 0 0 0))
;;; Generated autoloads from pdf-view.el

(autoload 'pdf-view-bookmark-jump-handler "pdf-view" "\
The bookmark handler-function interface for bookmark BMK.

See also `pdf-view-bookmark-make-record'.

\(fn BMK)" nil nil)

(register-definition-prefixes "pdf-view" '("pdf-view-"))

;;;***

;;;### (autoloads nil "pdf-virtual" "pdf-virtual.el" (0 0 0 0))
;;; Generated autoloads from pdf-virtual.el

(autoload 'pdf-virtual-edit-mode "pdf-virtual" "\
Major mode when editing a virtual PDF buffer.

\(fn)" t nil)

(autoload 'pdf-virtual-view-mode "pdf-virtual" "\
Major mode in virtual PDF buffers.

\(fn)" t nil)

(defvar pdf-virtual-global-minor-mode nil "\
Non-nil if Pdf-Virtual-Global minor mode is enabled.
See the `pdf-virtual-global-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pdf-virtual-global-minor-mode'.")

(custom-autoload 'pdf-virtual-global-minor-mode "pdf-virtual" nil)

(autoload 'pdf-virtual-global-minor-mode "pdf-virtual" "\
Enable recognition and handling of VPDF files.

If called interactively, toggle `Pdf-Virtual-Global minor mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'pdf-virtual-buffer-create "pdf-virtual" "\


\(fn &optional FILENAMES BUFFER-NAME DISPLAY-P)" t nil)

(register-definition-prefixes "pdf-virtual" '("pdf-virtual-"))

;;;***

;;;### (autoloads nil nil ("pdf-tools-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pdf-tools-autoloads.el ends here
;;; pdf-view-restore-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pdf-view-restore" "pdf-view-restore.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from pdf-view-restore.el

(autoload 'pdf-view-restore-mode "pdf-view-restore" "\
Automatically restore last known pdf position

If called interactively, toggle `Pdf-View-Restore mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "pdf-view-restore" '("pdf-view-restore" "use-file-base-name-flag"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pdf-view-restore-autoloads.el ends here
;;; pfuture-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pfuture" "pfuture.el" (0 0 0 0))
;;; Generated autoloads from pfuture.el

(autoload 'pfuture-new "pfuture" "\
Create a new future process for command CMD.
Any arguments after the command are interpreted as arguments to the command.
This will return a process object with additional 'stderr and 'stdout
properties, which can be read via (process-get process 'stdout) and
\(process-get process 'stderr) or alternatively with
\(pfuture-result process) or (pfuture-stderr process).

Note that CMD must be a *sequence* of strings, meaning
this is wrong: (pfuture-new \"git status\")
this is right: (pfuture-new \"git\" \"status\")

\(fn &rest CMD)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pfuture" '("pfuture-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pfuture-autoloads.el ends here
;;; pkg-info-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pkg-info" "pkg-info.el" (0 0 0 0))
;;; Generated autoloads from pkg-info.el

(autoload 'pkg-info-library-original-version "pkg-info" "\
Get the original version in the header of LIBRARY.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no X-Original-Version
header.

See Info node `(elisp)Library Headers' for more information
about library headers.

\(fn LIBRARY &optional SHOW)" t nil)

(autoload 'pkg-info-library-version "pkg-info" "\
Get the version in the header of LIBRARY.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no proper header.

See Info node `(elisp)Library Headers' for more information
about library headers.

\(fn LIBRARY &optional SHOW)" t nil)

(autoload 'pkg-info-defining-library-original-version "pkg-info" "\
Get the original version of the library defining FUNCTION.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

\(fn FUNCTION &optional SHOW)" t nil)

(autoload 'pkg-info-defining-library-version "pkg-info" "\
Get the version of the library defining FUNCTION.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

\(fn FUNCTION &optional SHOW)" t nil)

(autoload 'pkg-info-package-version "pkg-info" "\
Get the version of an installed PACKAGE.

If SHOW is non-nil, show the version in the minibuffer.

Return the version as list, or nil if PACKAGE is not installed.

\(fn PACKAGE &optional SHOW)" t nil)

(autoload 'pkg-info-version-info "pkg-info" "\
Obtain complete version info for LIBRARY and PACKAGE.

LIBRARY is a symbol denoting a named feature, or a library name
as string.  PACKAGE is a symbol denoting an ELPA package.  If
omitted or nil, default to LIBRARY.

If SHOW is non-nil, show the version in the minibuffer.

When called interactively, prompt for LIBRARY.  When called
interactively with prefix argument, prompt for PACKAGE as well.

Return a string with complete version information for LIBRARY.
This version information contains the version from the headers of
LIBRARY, and the version of the installed PACKAGE, the LIBRARY is
part of.  If PACKAGE is not installed, or if the PACKAGE version
is the same as the LIBRARY version, do not include a package
version.

\(fn LIBRARY &optional PACKAGE SHOW)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pkg-info" '("pkg-info-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pkg-info-autoloads.el ends here
;;; popup-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "popup" "popup.el" (0 0 0 0))
;;; Generated autoloads from popup.el

(register-definition-prefixes "popup" '("popup-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; popup-autoloads.el ends here
;;; pos-tip-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pos-tip" "pos-tip.el" (0 0 0 0))
;;; Generated autoloads from pos-tip.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pos-tip" '("pos-tip-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pos-tip-autoloads.el ends here
;;; posframe-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "posframe" "posframe.el" (0 0 0 0))
;;; Generated autoloads from posframe.el

(autoload 'posframe-workable-p "posframe" "\
Test posframe workable status." nil nil)

(autoload 'posframe-show "posframe" "\
Pop up a posframe and show STRING at POSITION.

POSITION can be:
1. An integer, meaning point position.
2. A cons of two integers, meaning absolute X and Y coordinates.
3. Other type, in which case the corresponding POSHANDLER should be
   provided.

POSHANDLER is a function of one argument returning an actual
position.  Its argument is a plist of the following form:

  (:position xxx
   :position-info xxx
   :poshandler xxx
   :font-height xxx
   :font-width xxx
   :posframe xxx
   :posframe-width xxx
   :posframe-height xxx
   :posframe-buffer xxx
   :parent-frame xxx
   :parent-window-left xxx
   :parent-window-top xxx
   :parent-frame-width xxx
   :parent-frame-height xxx
   :parent-window xxx
   :parent-window-width  xxx
   :parent-window-height xxx
   :minibuffer-height xxx
   :mode-line-height  xxx
   :header-line-height xxx
   :tab-line-height xxx
   :x-pixel-offset xxx
   :y-pixel-offset xxx)

By default, poshandler is auto-selected based on the type of POSITION,
but the selection can be overridden using the POSHANDLER argument.
The builtin poshandler functions are listed below:

1.  `posframe-poshandler-frame-center'
2.  `posframe-poshandler-frame-top-center'
3.  `posframe-poshandler-frame-top-left-corner'
4.  `posframe-poshandler-frame-top-right-corner'
5.  `posframe-poshandler-frame-bottom-center'
6.  `posframe-poshandler-frame-bottom-left-corner'
7.  `posframe-poshandler-frame-bottom-right-corner'
8.  `posframe-poshandler-window-center'
9.  `posframe-poshandler-window-top-center'
10. `posframe-poshandler-window-top-left-corner'
11. `posframe-poshandler-window-top-right-corner'
12. `posframe-poshandler-window-bottom-center'
13. `posframe-poshandler-window-bottom-left-corner'
14. `posframe-poshandler-window-bottom-right-corner'
15. `posframe-poshandler-point-top-left-corner'
16. `posframe-poshandler-point-bottom-left-corner'
17. `posframe-poshandler-point-bottom-left-corner-upward'
18. `posframe-poshandler-point-window-center'

by the way, poshandler can be used by other packages easily
\(for example: mini-frame) with the help of function
`posframe-poshandler-argbuilder'. like:

   (let* ((info (posframe-poshandler-argbuilder child-frame))
          (posn (posframe-poshandler-window-center info)))
     `((left . ,(car posn))
       (top . ,(cdr posn))))

POSHANDLER-EXTRA-INFO is a plist, which will prepend to the
argument of poshandler function: 'info', it will *OVERRIDE* the
exist key in 'info'.

This posframe's buffer is BUFFER-OR-NAME, which can be a buffer
or a name of a (possibly nonexistent) buffer.

If NO-PROPERTIES is non-nil, The STRING's properties will
be removed before being shown in posframe.

WIDTH, MIN-WIDTH, HEIGHT and MIN-HEIGHT, specify bounds on the
new total size of posframe.  MIN-HEIGHT and MIN-WIDTH default to
the values of ‘window-min-height’ and ‘window-min-width’
respectively.  These arguments are specified in the canonical
character width and height of posframe.

If LEFT-FRINGE or RIGHT-FRINGE is a number, left fringe or
right fringe with be shown with the specified width.

By default, posframe shows no borders, but users can specify
borders by setting BORDER-WIDTH to a positive number.  Border
color can be specified by BORDER-COLOR.

INTERNAL-BORDER-WIDTH and INTERNAL-BORDER-COLOR are same as
BORDER-WIDTH and INTERNAL-BORDER-COLOR, but do not suggest to use
for the reason:

   Add distinct controls for child frames' borders (Bug#45620)
   http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=ff7b1a133bfa7f2614650f8551824ffaef13fadc

Posframe's font as well as foreground and background colors are
derived from the current frame by default, but can be overridden
using the FONT, FOREGROUND-COLOR and BACKGROUND-COLOR arguments,
respectively.

By default, posframe will display no header-line, mode-line and
tab-line.  In case a header-line, mode-line or tab-line is
desired, users can set RESPECT-HEADER-LINE and RESPECT-MODE-LINE
to t.

INITIALIZE is a function with no argument.  It will run when
posframe buffer is first selected with `with-current-buffer'
in `posframe-show', and only run once (for performance reasons).

If LINES-TRUNCATE is non-nil, then lines will truncate in the
posframe instead of wrap.

OVERRIDE-PARAMETERS is very powful, *all* the frame parameters
used by posframe's frame can be overridden by it.

TIMEOUT can specify the number of seconds after which the posframe
will auto-hide.

If REFRESH is a number, posframe's frame-size will be re-adjusted
every REFRESH seconds.

When ACCEPT-FOCUS is non-nil, posframe will accept focus.
be careful, you may face some bugs when set it to non-nil.

HIDEHANDLER is a function, when it return t, posframe will be
hide when `post-command-hook' is executed, this function has a
plist argument:

  (:posframe-buffer xxx
   :posframe-parent-buffer xxx)

The builtin hidehandler functions are listed below:

1. `posframe-hidehandler-when-buffer-switch'


You can use `posframe-delete-all' to delete all posframes.

\(fn BUFFER-OR-NAME &key STRING POSITION POSHANDLER POSHANDLER-EXTRA-INFO WIDTH HEIGHT MIN-WIDTH MIN-HEIGHT X-PIXEL-OFFSET Y-PIXEL-OFFSET LEFT-FRINGE RIGHT-FRINGE BORDER-WIDTH BORDER-COLOR INTERNAL-BORDER-WIDTH INTERNAL-BORDER-COLOR FONT FOREGROUND-COLOR BACKGROUND-COLOR RESPECT-HEADER-LINE RESPECT-MODE-LINE INITIALIZE NO-PROPERTIES KEEP-RATIO LINES-TRUNCATE OVERRIDE-PARAMETERS TIMEOUT REFRESH ACCEPT-FOCUS HIDEHANDLER &allow-other-keys)" nil nil)

(autoload 'posframe-hide-all "posframe" "\
Hide all posframe frames." t nil)

(autoload 'posframe-delete-all "posframe" "\
Delete all posframe frames and buffers." t nil)

(register-definition-prefixes "posframe" '("posframe-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; posframe-autoloads.el ends here
;;; prescient-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "prescient" "prescient.el" (0 0 0 0))
;;; Generated autoloads from prescient.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "prescient" '("prescient-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; prescient-autoloads.el ends here
;;; pyim-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pyim" "pyim.el" (0 0 0 0))
;;; Generated autoloads from pyim.el

(defvar pyim-titles '("PYIM " "PYIM-EN " "PYIM-AU ") "\
Pyim 在 mode-line 中显示的名称.")

(register-input-method "pyim" "euc-cn" 'pyim-start (nth 0 pyim-titles))

(autoload 'pyim-start "pyim" "\
pyim 启动函数.
  TODO: Document NAME ACTIVE-FUNC RESTART SAVE-PERSONAL-DCACHE REFRESH-COMMON-DCACHE

pyim 是使用 `pyim-start' 来启动输入法，这个命令主要做如下工作：
1. 重置 `pyim-local-variable-list' 中所有的 local 变量。
2. 使用 `pyim-cchar2pinyin-create-cache' 创建汉字到拼音的 hash table 对应表。
3. 运行hook： `pyim-load-hook'。
4. 将 `pyim-dcache-save-caches' 命令添加到 `kill-emacs-hook' , emacs 关闭
之前将用户选择过的词生成的缓存和词频缓存保存到文件，供以后使用。
5. 设定变量：
1. `input-method-function'
2. `deactivate-current-input-method-function'
6. 运行 `pyim-active-hook'

pyim 使用函数 `pyim-start' 启动输入法的时候，会将变量
`input-method-function' 设置为 `pyim-input-method' ，这个变量会影
响 `read-event' 的行为。

当输入字符时，`read-event' 会被调用，`read-event' 调用的过程中，
会执行 `pyim-input-method' 这个函数。`pyim-input-method' 又调用函
数`pyim-start-translation'.

\(fn NAME &optional ACTIVE-FUNC RESTART SAVE-PERSONAL-DCACHE REFRESH-COMMON-DCACHE)" t nil)

(autoload 'pyim-convert-string-at-point "pyim" "\
将光标前的用户输入的字符串转换为中文.

如果 RETURN-CREGEXP 为真, pyim 会把用户输入的字符串当作
拼音，依照这个拼音来构建一个 regexp, 用户可以用这个 regexp
搜索拼音对应的汉字。

\(fn &optional RETURN-CREGEXP)" t nil)

(defvar pyim-isearch-mode nil "\
Non-nil if Pyim-Isearch mode is enabled.
See the `pyim-isearch-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyim-isearch-mode'.")

(custom-autoload 'pyim-isearch-mode "pyim" nil)

(autoload 'pyim-isearch-mode "pyim" "\
这个 mode 为 isearch 添加拼音搜索功能.

If called interactively, toggle `Pyim-Isearch mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'pyim-hanzi2pinyin "pyim" "\
将汉字字符串转换为对应的拼音字符串的工具.

如果 SHOU-ZI-MU 设置为 t, 转换仅得到拼音首字母字符串。当
RETURN-LIST 设置为 t 时，返回一个拼音列表，这个列表包含词条的一个
或者多个拼音（词条包含多音字时）；如果 IGNORE-DUO-YIN-ZI 设置为
t, 遇到多音字时，只使用第一个拼音，其它拼音忽略；当
ADJUST-DUO-YIN-Zi 设置为 t 时, `pyim-hanzi2pinyin' 会使用 pyim 已
安装的词库来校正多音字，但这个功能有一定的限制:

1. pyim 普通词库中不存在的词条不能较正
2. 多音字校正速度比较慢，实时转换会产生卡顿。

BUG: 当 STRING 中包含其它标点符号，并且设置 SEPERATER 时，结果会
包含多余的连接符：比如： '你=好' --> 'ni-=-hao'

\(fn STRING &optional SHOU-ZI-MU SEPARATOR RETURN-LIST IGNORE-DUO-YIN-ZI ADJUST-DUO-YIN-ZI)" nil nil)

(autoload 'pyim-hanzi2pinyin-simple "pyim" "\
简化版的 `pyim-hanzi2pinyin', 不处理多音字。

\(fn STRING &optional SHOU-ZI-MU SEPARATOR RETURN-LIST)" nil nil)

(autoload 'pyim-dicts-manager "pyim" "\
pyim 词库管理器。

使用这个词库管理器可以方便的执行下列命令：
1. 添加词库。
2. 删除词库。
3. 向上和向下移动词库。
4. 保存词库设置。
5. 重启输入法。" t nil)

(register-definition-prefixes "pyim" '("pyim-"))

;;;***

;;;### (autoloads nil "pyim-common" "pyim-common.el" (0 0 0 0))
;;; Generated autoloads from pyim-common.el

(register-definition-prefixes "pyim-common" '("pyim-"))

;;;***

;;;### (autoloads nil "pyim-dhashcache" "pyim-dhashcache.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from pyim-dhashcache.el

(register-definition-prefixes "pyim-dhashcache" '("pyim-dhashcache-"))

;;;***

;;;### (autoloads nil "pyim-dregcache" "pyim-dregcache.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from pyim-dregcache.el

(register-definition-prefixes "pyim-dregcache" '("pyim-dregcache-"))

;;;***

;;;### (autoloads nil "pyim-liberime" "pyim-liberime.el" (0 0 0 0))
;;; Generated autoloads from pyim-liberime.el

(register-definition-prefixes "pyim-liberime" '("pyim-"))

;;;***

;;;### (autoloads nil "pyim-probe" "pyim-probe.el" (0 0 0 0))
;;; Generated autoloads from pyim-probe.el

(register-definition-prefixes "pyim-probe" '("pyim-probe-"))

;;;***

;;;### (autoloads nil "pyim-pymap" "pyim-pymap.el" (0 0 0 0))
;;; Generated autoloads from pyim-pymap.el

(register-definition-prefixes "pyim-pymap" '("pyim-pymap"))

;;;***

;;;### (autoloads nil nil ("pyim-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pyim-autoloads.el ends here
;;; pyim-basedict-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pyim-basedict" "pyim-basedict.el" (0 0 0 0))
;;; Generated autoloads from pyim-basedict.el

(autoload 'pyim-basedict-enable "pyim-basedict" "\
Add basedict to pyim." t nil)

(register-definition-prefixes "pyim-basedict" '("pyim-basedict-"))

;;;***

;;;### (autoloads nil nil ("pyim-basedict-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pyim-basedict-autoloads.el ends here
;;; pyvenv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pyvenv" "pyvenv.el" (0 0 0 0))
;;; Generated autoloads from pyvenv.el

(autoload 'pyvenv-activate "pyvenv" "\
Activate the virtual environment in DIRECTORY.

\(fn DIRECTORY)" t nil)

(autoload 'pyvenv-deactivate "pyvenv" "\
Deactivate any current virtual environment." t nil)

(autoload 'pyvenv-workon "pyvenv" "\
Activate a virtual environment from $WORKON_HOME.

If the virtual environment NAME is already active, this function
does not try to reactivate the environment.

\(fn NAME)" t nil)

(defvar pyvenv-mode nil "\
Non-nil if Pyvenv mode is enabled.
See the `pyvenv-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyvenv-mode'.")

(custom-autoload 'pyvenv-mode "pyvenv" nil)

(autoload 'pyvenv-mode "pyvenv" "\
Global minor mode for pyvenv.

If called interactively, enable Pyvenv mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

Will show the current virtualenv in the mode line, and respect a
`pyvenv-workon' setting in files.

\(fn &optional ARG)" t nil)

(defvar pyvenv-tracking-mode nil "\
Non-nil if Pyvenv-Tracking mode is enabled.
See the `pyvenv-tracking-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyvenv-tracking-mode'.")

(custom-autoload 'pyvenv-tracking-mode "pyvenv" nil)

(autoload 'pyvenv-tracking-mode "pyvenv" "\
Global minor mode to track the current virtualenv.

If called interactively, enable Pyvenv-Tracking mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

When this mode is active, pyvenv will activate a buffer-specific
virtualenv whenever the user switches to a buffer with a
buffer-local `pyvenv-workon' or `pyvenv-activate' variable.

\(fn &optional ARG)" t nil)

(autoload 'pyvenv-restart-python "pyvenv" "\
Restart Python inferior processes." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pyvenv" '("pyvenv-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pyvenv-autoloads.el ends here
;;; queue-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "queue" "queue.el" (0 0 0 0))
;;; Generated autoloads from queue.el

(defalias 'make-queue 'queue-create "\
Create an empty queue data structure.")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "queue" '("queue")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; queue-autoloads.el ends here
;;; quickrun-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "quickrun" "quickrun.el" (0 0 0 0))
;;; Generated autoloads from quickrun.el

(autoload 'quickrun-set-default "quickrun" "\
Set `key' as default key in programing language `lang'.

\(fn LANG KEY)" nil nil)

(autoload 'quickrun-add-command "quickrun" "\
Not documented.

\(fn KEY ALIST &key DEFAULT MODE OVERRIDE)" nil nil)

(function-put 'quickrun-add-command 'lisp-indent-function 'defun)

(autoload 'quickrun "quickrun" "\
Run commands quickly for current buffer
   With universal prefix argument(C-u), select command-key,
   With double prefix argument(C-u C-u), run in compile-only-mode.

\(fn &rest PLIST)" t nil)

(autoload 'quickrun-with-arg "quickrun" "\
Run commands quickly for current buffer with arguments.

\(fn ARG)" t nil)

(autoload 'quickrun-region "quickrun" "\
Run commands with specified region.

\(fn START END)" t nil)

(autoload 'quickrun-replace-region "quickrun" "\
Run commands with specified region and replace.

\(fn START END)" t nil)

(autoload 'quickrun-eval-print "quickrun" "\
Run commands with specified region and replace.

\(fn START END)" t nil)

(autoload 'quickrun-compile-only "quickrun" "\
Exec only compilation." t nil)

(autoload 'quickrun-shell "quickrun" "\
Run commands in shell for interactive programs." t nil)

(autoload 'quickrun-autorun-mode "quickrun" "\
`quickrun' after saving buffer.

If called interactively, toggle `Quickrun-Autorun mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'anything-quickrun "quickrun" "\
Run quickrun with `anything'." t nil)

(autoload 'helm-quickrun "quickrun" "\
Run quickrun with `helm'." t nil)

(register-definition-prefixes "quickrun" '("helm-quickrun-" "quick"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; quickrun-autoloads.el ends here
;;; racket-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "racket-browse-url" "racket-browse-url.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-browse-url.el

(register-definition-prefixes "racket-browse-url" '("racket-browse-url"))

;;;***

;;;### (autoloads nil "racket-bug-report" "racket-bug-report.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-bug-report.el

(autoload 'racket-bug-report "racket-bug-report" "\
Fill a buffer with data to make a Racket Mode bug report." t nil)

;;;***

;;;### (autoloads nil "racket-cmd" "racket-cmd.el" (0 0 0 0))
;;; Generated autoloads from racket-cmd.el

(defvar racket-start-back-end-hook nil "\
Hook run after `racket-start-back-end'.")

(autoload 'racket-start-back-end "racket-cmd" "\
Start the back end process used by Racket Mode.

If the process is already started, this command will stop and restart it.

As the final step, runs the hook `racket-start-back-end-hook'." t nil)

(autoload 'racket-stop-back-end "racket-cmd" "\
Stop the back end process used by Racket Mode.

If the process is not already started, this does nothing." t nil)

(register-definition-prefixes "racket-cmd" '("racket-"))

;;;***

;;;### (autoloads nil "racket-collection" "racket-collection.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-collection.el

(register-definition-prefixes "racket-collection" '("racket-"))

;;;***

;;;### (autoloads nil "racket-common" "racket-common.el" (0 0 0 0))
;;; Generated autoloads from racket-common.el

(register-definition-prefixes "racket-common" '("racket-"))

;;;***

;;;### (autoloads nil "racket-complete" "racket-complete.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from racket-complete.el

(register-definition-prefixes "racket-complete" '("racket--"))

;;;***

;;;### (autoloads nil "racket-custom" "racket-custom.el" (0 0 0 0))
;;; Generated autoloads from racket-custom.el

(register-definition-prefixes "racket-custom" '("defface-racket" "racket-"))

;;;***

;;;### (autoloads nil "racket-debug" "racket-debug.el" (0 0 0 0))
;;; Generated autoloads from racket-debug.el

(autoload 'racket--debug-send-definition "racket-debug" "\


\(fn BEG END)" nil nil)

(autoload 'racket--debug-on-break "racket-debug" "\


\(fn RESPONSE)" nil nil)

(register-definition-prefixes "racket-debug" '("racket-"))

;;;***

;;;### (autoloads nil "racket-describe" "racket-describe.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from racket-describe.el

(register-definition-prefixes "racket-describe" '("racket-"))

;;;***

;;;### (autoloads nil "racket-doc" "racket-doc.el" (0 0 0 0))
;;; Generated autoloads from racket-doc.el

(register-definition-prefixes "racket-doc" '("racket--"))

;;;***

;;;### (autoloads nil "racket-edit" "racket-edit.el" (0 0 0 0))
;;; Generated autoloads from racket-edit.el

(add-to-list 'hs-special-modes-alist '(racket-mode "(" ")" ";" nil nil))

(register-definition-prefixes "racket-edit" '("racket-"))

;;;***

;;;### (autoloads nil "racket-eldoc" "racket-eldoc.el" (0 0 0 0))
;;; Generated autoloads from racket-eldoc.el

(register-definition-prefixes "racket-eldoc" '("racket--do-eldoc"))

;;;***

;;;### (autoloads nil "racket-font-lock" "racket-font-lock.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from racket-font-lock.el

(register-definition-prefixes "racket-font-lock" '("racket-"))

;;;***

;;;### (autoloads nil "racket-imenu" "racket-imenu.el" (0 0 0 0))
;;; Generated autoloads from racket-imenu.el

(register-definition-prefixes "racket-imenu" '("racket-"))

;;;***

;;;### (autoloads nil "racket-indent" "racket-indent.el" (0 0 0 0))
;;; Generated autoloads from racket-indent.el

(register-definition-prefixes "racket-indent" '("racket-"))

;;;***

;;;### (autoloads nil "racket-keywords-and-builtins" "racket-keywords-and-builtins.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-keywords-and-builtins.el

(register-definition-prefixes "racket-keywords-and-builtins" '("racket-"))

;;;***

;;;### (autoloads nil "racket-logger" "racket-logger.el" (0 0 0 0))
;;; Generated autoloads from racket-logger.el

(register-definition-prefixes "racket-logger" '("racket-"))

;;;***

;;;### (autoloads nil "racket-mode" "racket-mode.el" (0 0 0 0))
;;; Generated autoloads from racket-mode.el

(autoload 'racket-mode "racket-mode" "\
Major mode for editing Racket source files.

\\{racket-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

(add-to-list 'auto-mode-alist '("\\.rktd\\'" . racket-mode))

(add-to-list 'auto-mode-alist '("\\.rktl\\'" . racket-mode))

(modify-coding-system-alist 'file "\\.rkt[dl]?\\'" 'utf-8)

(add-to-list 'interpreter-mode-alist '("racket" . racket-mode))

(autoload 'racket-mode-start-faster "racket-mode" "\
Compile Racket Mode's .rkt files for faster startup.

Racket Mode is implemented as an Emacs Lisp \"front end\" that
talks to a Racket process \"back end\". Because Racket Mode is
delivered as an Emacs package instead of a Racket package,
installing it does not do the `raco setup` that is normally done
for Racket packages.

This command will do a `raco make` of Racket Mode's .rkt files,
creating bytecode files in `compiled/` subdirectories. As a
result, when a command must start the Racket process, it will
start somewhat faster.

On many computers, the resulting speed up is negligible, and
might not be worth the complication.

If you run this command, ever, you will need to run it again
after:

- Installing an updated version of Racket Mode. Otherwise, you
  might lose some of the speed-up.

- Installing a new version of Racket and/or changing the value of
  the variable `racket-program'. Otherwise, you might get an
  error message due to the bytecode being different versions.

To revert to compiling on startup, use
`racket-mode-start-slower'. " t nil)

(register-definition-prefixes "racket-mode" '("racket-"))

;;;***

;;;### (autoloads nil "racket-parens" "racket-parens.el" (0 0 0 0))
;;; Generated autoloads from racket-parens.el

(register-definition-prefixes "racket-parens" '("racket-"))

;;;***

;;;### (autoloads nil "racket-ppss" "racket-ppss.el" (0 0 0 0))
;;; Generated autoloads from racket-ppss.el

(register-definition-prefixes "racket-ppss" '("racket--ppss-"))

;;;***

;;;### (autoloads nil "racket-profile" "racket-profile.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from racket-profile.el

(register-definition-prefixes "racket-profile" '("racket-"))

;;;***

;;;### (autoloads nil "racket-repl" "racket-repl.el" (0 0 0 0))
;;; Generated autoloads from racket-repl.el

(autoload 'racket-repl "racket-repl" "\
Show a Racket REPL buffer in some window.

*IMPORTANT*

The main, intended use of Racket Mode's REPL is that you
`find-file' some specific .rkt file, then `racket-run' it. The
REPL will then match that file.

If the REPL isn't running, and you want to start it for no file
in particular? Then you could use this command. But the resulting
REPL will have a minimal \"#lang racket/base\" namespace. You
could enter \"(require racket)\" if you want the equivalent of
\"#lang racket\". You could also \"(require racket/enter)\" if
you want things like \"enter!\". But in some sense you'd be
\"using it wrong\". If you really don't want to use Racket Mode's
REPL as intended, then you might as well use a plain Emacs shell
buffer to run command-line Racket.

\(fn &optional NOSELECT)" t nil)

(autoload 'racket-run "racket-repl" "\
Save the buffer in REPL and run your program.

As well as evaluating the outermost, file module, automatically
runs the submodules specified by the customization variable
`racket-submodules-to-run'.

See also `racket-run-module-at-point', which runs just the
specific module at point.

With \\[universal-argument] uses errortrace for improved stack traces.
Otherwise follows the `racket-error-context' setting.

With \\[universal-argument] \\[universal-argument] instruments
code for step debugging. See `racket-debug-mode' and the variable
`racket-debuggable-files'.

Each run occurs within a Racket custodian. Any prior run's
custodian is shut down, releasing resources like threads and
ports. Each run's evaluation environment is reset to the contents
of the source file. In other words, like Dr Racket, this provides
the benefit that your source file is the \"single source of
truth\". At the same time, the run gives you a REPL inside the
namespace of the module, giving you the ability to explore it
interactively. Any explorations are temporary, unless you also
make them to your source file, they will be lost on the next run.

See also `racket-run-and-switch-to-repl', which is even more like
Dr Racket's Run command because it selects the REPL window after
running.

In the `racket-repl-mode' buffer, output that describes a file
and position is automatically \"linkified\". Examples of such
text include:

- Racket error messages.
- rackunit test failure location messages.
- print representation of path objects.

To visit these locations, move point there and press RET or mouse
click. Or, use the standard `next-error' and `previous-error'
commands.

\(fn &optional PREFIX)" t nil)

(autoload 'racket-run-module-at-point "racket-repl" "\
Save the buffer and run the module at point.

Like `racket-run' but runs the innermost module around point,
which is determined textually by looking for \"module\",
\"module*\", or \"module+\" forms nested to any depth, else
simply the outermost, file module.

\(fn &optional PREFIX)" t nil)

(register-definition-prefixes "racket-repl" '("racket-" "with-racket-repl-buffer"))

;;;***

;;;### (autoloads nil "racket-repl-buffer-name" "racket-repl-buffer-name.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-repl-buffer-name.el

(autoload 'racket-repl-buffer-name-shared "racket-repl-buffer-name" "\
All `racket-mode' edit buffers share one `racket-repl-mode' buffer.

A value for the variable `racket-repl-buffer-name-function'." t nil)

(autoload 'racket-repl-buffer-name-unique "racket-repl-buffer-name" "\
Each `racket-mode' edit buffer gets its own `racket-repl-mode' buffer.

A value for the variable `racket-repl-buffer-name-function'." t nil)

(autoload 'racket-repl-buffer-name-project "racket-repl-buffer-name" "\
All `racket-mode' buffers in a project share a `racket-repl-mode' buffer.

A value for the variable `racket-repl-buffer-name-function'.

The \"project\" is determined by `racket-project-root'." t nil)

(register-definition-prefixes "racket-repl-buffer-name" '("racket-"))

;;;***

;;;### (autoloads nil "racket-show" "racket-show.el" (0 0 0 0))
;;; Generated autoloads from racket-show.el

(register-definition-prefixes "racket-show" '("racket-"))

;;;***

;;;### (autoloads nil "racket-smart-open" "racket-smart-open.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-smart-open.el

(autoload 'racket-smart-open-bracket-mode "racket-smart-open" "\
Minor mode to let you always type `[`' to insert `(` or `[` automatically.

If called interactively, toggle `Racket-Smart-Open-Bracket mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Behaves like the \"Automatically adjust opening square brackets\"
feature in Dr. Racket.

By default, inserts a `(`. Inserts a `[` in the following cases:

  - `let`-like bindings -- forms with `let` in the name as well
    as things like `parameterize`, `with-handlers`, and
    `with-syntax`.

  - `case`, `cond`, `match`, `syntax-case`, `syntax-parse`, and
    `syntax-rules` clauses.

  - `for`-like bindings and `for/fold` accumulators.

  - `class` declaration syntax, such as `init` and `inherit`.

When the previous s-expression in a sequence is a compound
expression, uses the same kind of delimiter.

To force insert `[`, use `quoted-insert'.

Combined with `racket-insert-closing' this means that you can
press the unshifted `[` and `]` keys to get whatever delimiters
follow the Racket conventions for these forms. When something
like `electric-pair-mode' or `paredit-mode' is active, you need
not even press `]`.

Tip: When also using `paredit-mode', enable that first so that
the binding for the `[`' key in the map for
`racket-smart-open-bracket-mode' has higher priority. See also
the variable `minor-mode-map-alist'.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "racket-smart-open" '("racket-"))

;;;***

;;;### (autoloads nil "racket-stepper" "racket-stepper.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from racket-stepper.el

(register-definition-prefixes "racket-stepper" '("racket-"))

;;;***

;;;### (autoloads nil "racket-unicode-input-method" "racket-unicode-input-method.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-unicode-input-method.el

(autoload 'racket-unicode-input-method-enable "racket-unicode-input-method" "\
Set input method to racket-unicode.

The racket-unicode input method lets you easily type various
Unicode symbols that might be useful when writing Racket code.

To automatically enable the racket-unicode input method in
racket-mode and racket-repl-mode buffers, put the following code
in your Emacs init file:

#+BEGIN_SRC elisp
    (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
    (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
#+END_SRC

To temporarily enable this input method for a single buffer you
can use \"M-x racket-unicode-input-method-enable\".

Use the standard Emacs key C-\\ to toggle the input method.

When the racket-unicode input method is active, you can for
example type \"All\" and it is immediately replaced with \"∀\". A
few other examples:

| omega     | ω                        |
| x_1       | x₁                       |
| x^1       | x¹                       |
| A         | 𝔸                        |
| test-->>E | test-->>∃ (racket/redex) |
| vdash     | ⊢                        |

To see a table of all key sequences use \"M-x
describe-input-method <RET> racket-unicode\".

If you want to add your own mappings to the \"racket-unicode\"
input method, you may add code like the following example in your
Emacs init file:

#+BEGIN_SRC elisp
    ;; Either (require 'racket-mode) here, or, if you use
    ;; use-package, put the code below in the :config section.
    (with-temp-buffer
      (racket-unicode-input-method-enable)
      (set-input-method \"racket-unicode\")
      (let ((quail-current-package (assoc \"racket-unicode\"
                                          quail-package-alist)))
        (quail-define-rules ((append . t))
                            (\"^o\" [\"ᵒ\"]))))
#+END_SRC

If you don’t like the highlighting of partially matching tokens you
can turn it off by setting `input-method-highlight-flag' to nil." t nil)

;;;***

;;;### (autoloads nil "racket-util" "racket-util.el" (0 0 0 0))
;;; Generated autoloads from racket-util.el

(register-definition-prefixes "racket-util" '("racket-"))

;;;***

;;;### (autoloads nil "racket-visit" "racket-visit.el" (0 0 0 0))
;;; Generated autoloads from racket-visit.el

(register-definition-prefixes "racket-visit" '("racket--"))

;;;***

;;;### (autoloads nil "racket-wsl" "racket-wsl.el" (0 0 0 0))
;;; Generated autoloads from racket-wsl.el

(register-definition-prefixes "racket-wsl" '("racket-"))

;;;***

;;;### (autoloads nil "racket-xp" "racket-xp.el" (0 0 0 0))
;;; Generated autoloads from racket-xp.el

(autoload 'racket-xp-mode "racket-xp" "\
A minor mode that analyzes expanded code to explain and explore.

If called interactively, toggle `Racket-Xp mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This minor mode is an optional enhancement to `racket-mode' edit
buffers. Like any minor mode, you can turn it on or off for a
specific buffer. If you always want to use it, put the following
code in your Emacs init file:

#+BEGIN_SRC elisp
    (require 'racket-xp)
    (add-hook 'racket-mode-hook #'racket-xp-mode)
#+END_SRC

Note: This mode won't do anything unless/until the Racket Mode
back end is running. It will try to start the back end
automatically. You do /not/ need to `racket-run' the buffer you
are editing.

This mode uses the drracket/check-syntax package to analyze
fully-expanded programs, without needing to evaluate a.k.a.
\"run\" them. The resulting analysis provides information for:

- Visually annotating bindings -- local or imported definitions
  and references to them.

- Visually annotating expressions in a tail position, as well as
  the enclosing expression with respect to which they are in a
  tail position.

- Completion candidates.

- Defintions' source and documentation.

When point is on a definition or use, related items are
highlighted using `racket-xp-def-face' and `racket-xp-use-face'
-- instead of drawing arrows as in Dr Racket. Information is
displayed using the function(s) in the hook variable
`racket-show-functions'; it is also available when hovering the
mouse cursor.

Note: If you find these point-motion features too distracting
and/or slow, in your `racket-xp-mode-hook' you may disable them:

#+BEGIN_SRC elisp
  (require 'racket-xp)
  (add-hook 'racket-xp-mode-hook
            (lambda ()
              (remove-hook 'pre-redisplay-functions
                           #'racket-xp-pre-redisplay
                           t)))
#+END_SRC

The remaining features discussed below will still work.

You may also use commands to navigate among a definition and its
uses, or to rename a local definitions and all its uses:

  - `racket-xp-next-definition'
  - `racket-xp-previous-definition'
  - `racket-xp-next-use'
  - `racket-xp-previous-use'

In the following little example, not only does
drracket/check-syntax distinguish the various \"x\" bindings, it
understands the two different imports of \"define\":

#+BEGIN_SRC racket
  #lang racket/base
  (define x 1)
  x
  (let ([x x])
    (+ x 1))
  (module m typed/racket/base
    (define x 2)
    x)
#+END_SRC

When point is on the opening parenthesis of an expression in tail
position, it is highlighted using the face
`racket-xp-tail-position-face'.

When point is on the opening parenthesis of an enclosing
expression with respect to which one or more expressions are in
tail position, it is highlighted using the face
`racket-xp-tail-target-face'.

Furthermore, when point is on the opening parenthesis of either
kind of expression, all of the immediately related expressions
are also highlighted. Various commands move among them:

  - `racket-xp-tail-up'
  - `racket-xp-tail-down'
  - `racket-xp-tail-next-sibling'
  - `racket-xp-tail-previous-sibling'

The function `racket-xp-complete-at-point' is added to the
variable `completion-at-point-functions'. Note that in this case,
it is not smart about submodules; identifiers are assumed to be
definitions from the file's module or its imports. In addition to
supplying completion candidates, it supports the
\":company-location\" property to inspect the definition of a
candidate and the \":company-doc-buffer\" property to view its
documentation.

When you edit the buffer, existing annotations are retained;
their positions are updated to reflect the edit. Annotations for
new or deleted text are not requested until after
`racket-xp-after-change-refresh-delay' seconds. The request is
made asynchronously so that Emacs will not block -- for
moderately complex source files, it can take some seconds simply
to fully expand them, as well as a little more time for the
drracket/check-syntax analysis. When the results are ready, all
annotations for the buffer are completely refreshed.

You may also set `racket-xp-after-change-refresh-delay' to nil
and use the `racket-xp-annotate' command manually.

The mode line changes to reflect the current status of
annotations, and whether or not you had a syntax error.

If you have one or more syntax errors, `racket-xp-next-error' and
`racket-xp-previous-error' navigate among them. Although most
languages will stop after the first syntax error, some like Typed
Racket will try to collect and report multiple errors.

You may use `xref-find-definitions' \\[xref-find-definitions],
`xref-pop-marker-stack' \\[xref-pop-marker-stack], and
`xref-find-references': `racket-xp-mode' adds a backend to the
variable `xref-backend-functions'. This backend uses information
from the drracket/check-syntax static analysis. Its ability to
find references is limited to the current file; when it finds
none it will try the default xref backend implementation which is
grep-based.

Tip: This mode follows the convention that a minor mode may only
use a prefix key consisting of \"C-c\" followed by a punctuation
key. As a result, `racket-xp-control-c-hash-keymap' is bound to
\"C-c #\" by default. Although you might find this awkward to
type, remember that as an Emacs user, you are free to bind this
map to a more convenient prefix, and/or bind any individual
commands directly to whatever keys you prefer.

\\{racket-xp-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "racket-xp" '("racket-"))

;;;***

;;;### (autoloads nil "racket-xp-complete" "racket-xp-complete.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-xp-complete.el

(register-definition-prefixes "racket-xp-complete" '("racket-"))

;;;***

;;;### (autoloads nil nil ("racket-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; racket-mode-autoloads.el ends here
;;; rainbow-delimiters-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rainbow-delimiters" "rainbow-delimiters.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rainbow-delimiters.el

(autoload 'rainbow-delimiters-mode "rainbow-delimiters" "\
Highlight nested parentheses, brackets, and braces according to their depth.

If called interactively, enable Rainbow-Delimiters mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'rainbow-delimiters-mode-enable "rainbow-delimiters" "\
Enable `rainbow-delimiters-mode'." nil nil)

(autoload 'rainbow-delimiters-mode-disable "rainbow-delimiters" "\
Disable `rainbow-delimiters-mode'." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rainbow-delimiters" '("rainbow-delimiters-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rainbow-delimiters-autoloads.el ends here
;;; request-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "request" "request.el" (0 0 0 0))
;;; Generated autoloads from request.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "request" '("request-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; request-autoloads.el ends here
;;; rime-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rime" "rime.el" (0 0 0 0))
;;; Generated autoloads from rime.el

(defvar rime-title (char-to-string 12563) "\
The title of input method.")

(autoload 'rime-lighter "rime" "\
Return a lighter which can be used in mode-line.

The content is `rime-title'.

You can customize the color with `rime-indicator-face' and `rime-indicator-dim-face'." nil nil)

(autoload 'rime-activate "rime" "\
Activate rime.
Argument NAME ignored.

\(fn NAME)" nil nil)

(register-input-method "rime" "euc-cn" 'rime-activate rime-title)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rime" '("rime-")))

;;;***

;;;### (autoloads nil "rime-predicates" "rime-predicates.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from rime-predicates.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rime-predicates" '("rime-predicate-")))

;;;***

;;;### (autoloads nil nil ("rime-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rime-autoloads.el ends here
;;; ripgrep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ripgrep" "ripgrep.el" (0 0 0 0))
;;; Generated autoloads from ripgrep.el

(autoload 'ripgrep-regexp "ripgrep" "\
Run a ripgrep search with `REGEXP' rooted at `DIRECTORY'.
`ARGS' provides Ripgrep command line arguments.

\(fn REGEXP DIRECTORY &optional ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ripgrep" '("ripgrep")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ripgrep-autoloads.el ends here
;;; rjsx-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rjsx-mode" "rjsx-mode.el" (0 0 0 0))
;;; Generated autoloads from rjsx-mode.el

(autoload 'rjsx-mode "rjsx-mode" "\
Major mode for editing JSX files.

\(fn)" t nil)

(autoload 'rjsx-minor-mode "rjsx-mode" "\
Minor mode for parsing JSX syntax into an AST.

If called interactively, enable Rjsx minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

(autoload 'rjsx-comment-dwim "rjsx-mode" "\
RJSX implementation of `comment-dwim'. If called on a region,
this function delegates to `comment-or-uncomment-region'. If the
point is not in a JSX context, it delegates to the
`comment-dwim', otherwise it will comment the JSX AST node at
point using the apppriate comment delimiters.

For example: If point is on a JSX attribute or JSX expression, it
will comment the entire attribute using \"/* */\". , otherwise if
it's on a descendent JSX Element, it will use \"{/* */}\"
instead.

\(fn ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rjsx-mode" '("rjsx-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rjsx-mode-autoloads.el ends here
;;; rust-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rust-mode" "rust-mode.el" (0 0 0 0))
;;; Generated autoloads from rust-mode.el

(autoload 'rust-mode "rust-mode" "\
Major mode for Rust code.

\\{rust-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(autoload 'rust-dbg-wrap-or-unwrap "rust-mode" "\
Either remove or add the dbg! macro." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-mode" '("cargo-compilation-regexps" "rust")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rust-mode-autoloads.el ends here
;;; s-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "s" "s.el" (0 0 0 0))
;;; Generated autoloads from s.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "s" '("s-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; s-autoloads.el ends here
;;; scss-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "scss-mode" "scss-mode.el" (0 0 0 0))
;;; Generated autoloads from scss-mode.el

(autoload 'scss-mode "scss-mode" "\
Major mode for editing SCSS files, http://sass-lang.com/
Special commands:
\\{scss-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "scss-mode" '("flymake-scss-init" "scss-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; scss-mode-autoloads.el ends here
;;; seq-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "seq-24" "seq-24.el" (0 0 0 0))
;;; Generated autoloads from seq-24.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "seq-24" '("seq")))

;;;***

;;;### (autoloads nil "seq-25" "seq-25.el" (0 0 0 0))
;;; Generated autoloads from seq-25.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "seq-25" '("seq--when-emacs-25-p")))

;;;***

;;;### (autoloads nil nil ("seq-pkg.el" "seq.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; seq-autoloads.el ends here
;;; sesman-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sesman" "sesman.el" (0 0 0 0))
;;; Generated autoloads from sesman.el

(autoload 'sesman-start "sesman" "\
Start a Sesman session." t nil)

(autoload 'sesman-restart "sesman" "\
Restart sesman session.
When WHICH is nil, restart the current session; when a single universal
argument or 'linked, restart all linked sessions; when a double universal
argument, t or 'all, restart all sessions. For programmatic use, WHICH can also
be a session or a name of the session, in which case that session is restarted.

\(fn &optional WHICH)" t nil)

(autoload 'sesman-quit "sesman" "\
Terminate a Sesman session.
When WHICH is nil, kill only the current session; when a single universal
argument or 'linked, kill all linked sessions; when a double universal argument,
t or 'all, kill all sessions. For programmatic use, WHICH can also be a session
or a name of the session, in which case that session is killed.

\(fn &optional WHICH)" t nil)

(autoload 'sesman-info "sesman" "\
Display info for all current sessions (`sesman-current-sessions').
In the resulting minibuffer display linked sessions are numbered and the
other (friendly) sessions are not. When ALL is non-nil, show info for all
sessions.

\(fn &optional ALL)" t nil)

(autoload 'sesman-link-with-buffer "sesman" "\
Ask for SESSION and link with BUFFER.
BUFFER defaults to current buffer. On universal argument, or if BUFFER is 'ask,
ask for buffer.

\(fn &optional BUFFER SESSION)" t nil)

(autoload 'sesman-link-with-directory "sesman" "\
Ask for SESSION and link with DIR.
DIR defaults to `default-directory'. On universal argument, or if DIR is 'ask,
ask for directory.

\(fn &optional DIR SESSION)" t nil)

(autoload 'sesman-link-with-project "sesman" "\
Ask for SESSION and link with PROJECT.
PROJECT defaults to current project. On universal argument, or if PROJECT is
'ask, ask for the project. SESSION defaults to the current session.

\(fn &optional PROJECT SESSION)" t nil)

(autoload 'sesman-link-with-least-specific "sesman" "\
Ask for SESSION and link with the least specific context available.
Normally the least specific context is the project. If not in a project, link
with the `default-directory'. If `default-directory' is nil, link with current
buffer.

\(fn &optional SESSION)" t nil)

(autoload 'sesman-unlink "sesman" "\
Break any of the previously created links." t nil)
 (autoload 'sesman-map "sesman" "Session management prefix keymap." t 'keymap)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sesman" '("sesman-")))

;;;***

;;;### (autoloads nil "sesman-browser" "sesman-browser.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from sesman-browser.el

(autoload 'sesman-browser "sesman-browser" "\
Display an interactive session browser.
See `sesman-browser-mode' for more details." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sesman-browser" '("sesman-")))

;;;***

;;;### (autoloads nil nil ("sesman-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sesman-autoloads.el ends here
;;; shrink-path-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "shrink-path" "shrink-path.el" (0 0 0 0))
;;; Generated autoloads from shrink-path.el

(register-definition-prefixes "shrink-path" '("shrink-path-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; shrink-path-autoloads.el ends here
;;; sly-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sly" "sly.el" (0 0 0 0))
;;; Generated autoloads from sly.el

(define-obsolete-variable-alias 'sly-setup-contribs 'sly-contribs "2.3.2")

(defvar sly-contribs '(sly-fancy) "\
A list of contrib packages to load with SLY.")

(autoload 'sly-setup "sly" "\
Have SLY load and use extension modules CONTRIBS.
CONTRIBS defaults to `sly-contribs' and is a list (LIB1 LIB2...)
symbols of `provide'd and `require'd Elisp libraries.

If CONTRIBS is nil, `sly-contribs' is *not* affected, otherwise
it is set to CONTRIBS.

However, after `require'ing LIB1, LIB2 ..., this command invokes
additional initialization steps associated with each element
LIB1, LIB2, which can theoretically be reverted by
`sly-disable-contrib.'

Notably, one of the extra initialization steps is affecting the
value of `sly-required-modules' (which see) thus affecting the
libraries loaded in the Slynk servers.

If SLY is currently connected to a Slynk and a contrib in
CONTRIBS has never been loaded, that Slynk is told to load the
associated Slynk extension module.

To ensure that a particular contrib is loaded, use
`sly-enable-contrib' instead.

\(fn &optional CONTRIBS)" t nil)

(autoload 'sly-mode "sly" "\
Minor mode for horizontal SLY functionality.

If called interactively, enable Sly mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'sly-editing-mode "sly" "\
Minor mode for editing `lisp-mode' buffers.

If called interactively, enable Sly-Editing mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'sly "sly" "\
Start a Lisp implementation and connect to it.

  COMMAND designates a the Lisp implementation to start as an
\"inferior\" process to the Emacs process. It is either a
pathname string pathname to a lisp executable, a list (EXECUTABLE
ARGS...), or a symbol indexing
`sly-lisp-implementations'. CODING-SYSTEM is a symbol overriding
`sly-net-coding-system'.

Interactively, both COMMAND and CODING-SYSTEM are nil and the
prefix argument controls the precise behaviour:

- With no prefix arg, try to automatically find a Lisp.  First
  consult `sly-command-switch-to-existing-lisp' and analyse open
  connections to maybe switch to one of those.  If a new lisp is
  to be created, first lookup `sly-lisp-implementations', using
  `sly-default-lisp' as a default strategy.  Then try
  `inferior-lisp-program' if it looks like it points to a valid
  lisp.  Failing that, guess the location of a lisp
  implementation.

- With a positive prefix arg (one C-u), prompt for a command
  string that starts a Lisp implementation.

- With a negative prefix arg (M-- M-x sly, for example) prompt
  for a symbol indexing one of the entries in
  `sly-lisp-implementations'

\(fn &optional COMMAND CODING-SYSTEM INTERACTIVE)" t nil)

(autoload 'sly-connect "sly" "\
Connect to a running Slynk server. Return the connection.
With prefix arg, asks if all connections should be closed
before.

\(fn HOST PORT &optional CODING-SYSTEM INTERACTIVE-P)" t nil)

(autoload 'sly-hyperspec-lookup "sly" "\
A wrapper for `hyperspec-lookup'

\(fn SYMBOL-NAME)" t nil)

(autoload 'sly-info "sly" "\
Read SLY manual

\(fn FILE &optional NODE)" t nil)

(add-hook 'lisp-mode-hook 'sly-editing-mode)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sly" '("define-sly-" "inferior-lisp-program" "make-sly-" "sly-")))

;;;***

;;;### (autoloads nil nil ("sly-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sly-autoloads.el ends here
;;; smartparens-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "smartparens" "smartparens.el" (0 0 0 0))
;;; Generated autoloads from smartparens.el

(autoload 'sp-cheat-sheet "smartparens" "\
Generate a cheat sheet of all the smartparens interactive functions.

Without a prefix argument, print only the short documentation and examples.

With non-nil prefix argument ARG, show the full documentation for each function.

You can follow the links to the function or variable help page.
To get back to the full list, use \\[help-go-back].

You can use `beginning-of-defun' and `end-of-defun' to jump to
the previous/next entry.

Examples are fontified using the `font-lock-string-face' for
better orientation.

\(fn &optional ARG)" t nil)

(defvar smartparens-mode-map (make-sparse-keymap) "\
Keymap used for `smartparens-mode'.")

(autoload 'sp-use-paredit-bindings "smartparens" "\
Initiate `smartparens-mode-map' with `sp-paredit-bindings'." t nil)

(autoload 'sp-use-smartparens-bindings "smartparens" "\
Initiate `smartparens-mode-map' with `sp-smartparens-bindings'." t nil)

(autoload 'smartparens-mode "smartparens" "\
Toggle smartparens mode.

If called interactively, toggle `Smartparens mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

You can enable pre-set bindings by customizing
`sp-base-key-bindings' variable.  The current content of
`smartparens-mode-map' is:

 \\{smartparens-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'smartparens-strict-mode "smartparens" "\
Toggle the strict smartparens mode.

If called interactively, toggle `Smartparens-Strict mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When strict mode is active, `delete-char', `kill-word' and their
backward variants will skip over the pair delimiters in order to
keep the structure always valid (the same way as `paredit-mode'
does).  This is accomplished by remapping them to
`sp-delete-char' and `sp-kill-word'.  There is also function
`sp-kill-symbol' that deletes symbols instead of words, otherwise
working exactly the same (it is not bound to any key by default).

When strict mode is active, this is indicated with \"/s\"
after the smartparens indicator in the mode list.

\(fn &optional ARG)" t nil)

(put 'smartparens-global-strict-mode 'globalized-minor-mode t)

(defvar smartparens-global-strict-mode nil "\
Non-nil if Smartparens-Global-Strict mode is enabled.
See the `smartparens-global-strict-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smartparens-global-strict-mode'.")

(custom-autoload 'smartparens-global-strict-mode "smartparens" nil)

(autoload 'smartparens-global-strict-mode "smartparens" "\
Toggle Smartparens-Strict mode in all buffers.
With prefix ARG, enable Smartparens-Global-Strict mode if ARG is
positive; otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Smartparens-Strict mode is enabled in all buffers where
`turn-on-smartparens-strict-mode' would do it.

See `smartparens-strict-mode' for more information on
Smartparens-Strict mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-smartparens-strict-mode "smartparens" "\
Turn on `smartparens-strict-mode'." t nil)

(autoload 'turn-off-smartparens-strict-mode "smartparens" "\
Turn off `smartparens-strict-mode'." t nil)

(put 'smartparens-global-mode 'globalized-minor-mode t)

(defvar smartparens-global-mode nil "\
Non-nil if Smartparens-Global mode is enabled.
See the `smartparens-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smartparens-global-mode'.")

(custom-autoload 'smartparens-global-mode "smartparens" nil)

(autoload 'smartparens-global-mode "smartparens" "\
Toggle Smartparens mode in all buffers.
With prefix ARG, enable Smartparens-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG is
omitted or nil.

Smartparens mode is enabled in all buffers where
`turn-on-smartparens-mode' would do it.

See `smartparens-mode' for more information on Smartparens mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-smartparens-mode "smartparens" "\
Turn on `smartparens-mode'.

This function is used to turn on `smartparens-global-mode'.

By default `smartparens-global-mode' ignores buffers with
`mode-class' set to special, but only if they are also not comint
buffers.

Additionally, buffers on `sp-ignore-modes-list' are ignored.

You can still turn on smartparens in these mode manually (or
in mode's startup-hook etc.) by calling `smartparens-mode'." t nil)

(autoload 'turn-off-smartparens-mode "smartparens" "\
Turn off `smartparens-mode'." t nil)

(autoload 'show-smartparens-mode "smartparens" "\
Toggle visualization of matching pairs.  When enabled, any
matching pair is highlighted after `sp-show-pair-delay' seconds
of Emacs idle time if the point is immediately in front or after
a pair.  This mode works similarly to `show-paren-mode', but
support custom pairs.

If called interactively, toggle `Show-Smartparens mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'show-smartparens-global-mode 'globalized-minor-mode t)

(defvar show-smartparens-global-mode nil "\
Non-nil if Show-Smartparens-Global mode is enabled.
See the `show-smartparens-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `show-smartparens-global-mode'.")

(custom-autoload 'show-smartparens-global-mode "smartparens" nil)

(autoload 'show-smartparens-global-mode "smartparens" "\
Toggle Show-Smartparens mode in all buffers.
With prefix ARG, enable Show-Smartparens-Global mode if ARG is
positive; otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Show-Smartparens mode is enabled in all buffers where
`turn-on-show-smartparens-mode' would do it.

See `show-smartparens-mode' for more information on Show-Smartparens
mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-show-smartparens-mode "smartparens" "\
Turn on `show-smartparens-mode'." t nil)

(autoload 'turn-off-show-smartparens-mode "smartparens" "\
Turn off `show-smartparens-mode'." t nil)

(register-definition-prefixes "smartparens" '("smartparens-" "sp-"))

;;;***

;;;### (autoloads nil "smartparens-clojure" "smartparens-clojure.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-clojure.el

(register-definition-prefixes "smartparens-clojure" '("sp-clojure-prefix"))

;;;***

;;;### (autoloads nil "smartparens-config" "smartparens-config.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-config.el

(register-definition-prefixes "smartparens-config" '("sp-lisp-invalid-hyperlink-p"))

;;;***

;;;### (autoloads nil "smartparens-crystal" "smartparens-crystal.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-crystal.el

(register-definition-prefixes "smartparens-crystal" '("sp-crystal-"))

;;;***

;;;### (autoloads nil "smartparens-elixir" "smartparens-elixir.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-elixir.el

(register-definition-prefixes "smartparens-elixir" '("sp-elixir-"))

;;;***

;;;### (autoloads nil "smartparens-ess" "smartparens-ess.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from smartparens-ess.el

(register-definition-prefixes "smartparens-ess" '("sp-ess-"))

;;;***

;;;### (autoloads nil "smartparens-haskell" "smartparens-haskell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-haskell.el

(register-definition-prefixes "smartparens-haskell" '("sp-"))

;;;***

;;;### (autoloads nil "smartparens-html" "smartparens-html.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from smartparens-html.el

(register-definition-prefixes "smartparens-html" '("sp-html-"))

;;;***

;;;### (autoloads nil "smartparens-latex" "smartparens-latex.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-latex.el

(register-definition-prefixes "smartparens-latex" '("sp-latex-"))

;;;***

;;;### (autoloads nil "smartparens-lua" "smartparens-lua.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from smartparens-lua.el

(register-definition-prefixes "smartparens-lua" '("sp-lua-post-keyword-insert"))

;;;***

;;;### (autoloads nil "smartparens-markdown" "smartparens-markdown.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-markdown.el

(register-definition-prefixes "smartparens-markdown" '("sp-"))

;;;***

;;;### (autoloads nil "smartparens-org" "smartparens-org.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from smartparens-org.el

(register-definition-prefixes "smartparens-org" '("sp-"))

;;;***

;;;### (autoloads nil "smartparens-python" "smartparens-python.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-python.el

(register-definition-prefixes "smartparens-python" '("sp-python-"))

;;;***

;;;### (autoloads nil "smartparens-rst" "smartparens-rst.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from smartparens-rst.el

(register-definition-prefixes "smartparens-rst" '("sp-rst-point-after-backtick"))

;;;***

;;;### (autoloads nil "smartparens-ruby" "smartparens-ruby.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from smartparens-ruby.el

(register-definition-prefixes "smartparens-ruby" '("sp-"))

;;;***

;;;### (autoloads nil "smartparens-rust" "smartparens-rust.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from smartparens-rust.el

(register-definition-prefixes "smartparens-rust" '("sp-"))

;;;***

;;;### (autoloads nil "smartparens-scala" "smartparens-scala.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-scala.el

(register-definition-prefixes "smartparens-scala" '("sp-scala-wrap-with-indented-newlines"))

;;;***

;;;### (autoloads nil "smartparens-text" "smartparens-text.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from smartparens-text.el

(register-definition-prefixes "smartparens-text" '("sp-text-mode-"))

;;;***

;;;### (autoloads nil "sp-sublimetext-like" "sp-sublimetext-like.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from sp-sublimetext-like.el

(register-definition-prefixes "sp-sublimetext-like" '("sp-point-not-before-word"))

;;;***

;;;### (autoloads nil nil ("smartparens-c.el" "smartparens-javascript.el"
;;;;;;  "smartparens-ml.el" "smartparens-pkg.el" "smartparens-racket.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smartparens-autoloads.el ends here
;;; spinner-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "spinner" "spinner.el" (0 0 0 0))
;;; Generated autoloads from spinner.el

(autoload 'spinner-create "spinner" "\
Create a spinner of the given TYPE.
The possible TYPEs are described in `spinner--type-to-frames'.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

If BUFFER-LOCAL is non-nil, the spinner will be automatically
deactivated if the buffer is killed.  If BUFFER-LOCAL is a
buffer, use that instead of current buffer.

When started, in order to function properly, the spinner runs a
timer which periodically calls `force-mode-line-update' in the
curent buffer.  If BUFFER-LOCAL was set at creation time, then
`force-mode-line-update' is called in that buffer instead.  When
the spinner is stopped, the timer is deactivated.

DELAY, if given, is the number of seconds to wait after starting
the spinner before actually displaying it. It is safe to cancel
the spinner before this time, in which case it won't display at
all.

\(fn &optional TYPE BUFFER-LOCAL FPS DELAY)" nil nil)

(autoload 'spinner-start "spinner" "\
Start a mode-line spinner of given TYPE-OR-OBJECT.
If TYPE-OR-OBJECT is an object created with `make-spinner',
simply activate it.  This method is designed for minor modes, so
they can use the spinner as part of their lighter by doing:
    \\='(:eval (spinner-print THE-SPINNER))
To stop this spinner, call `spinner-stop' on it.

If TYPE-OR-OBJECT is anything else, a buffer-local spinner is
created with this type, and it is displayed in the
`mode-line-process' of the buffer it was created it.  Both
TYPE-OR-OBJECT and FPS are passed to `make-spinner' (which see).
To stop this spinner, call `spinner-stop' in the same buffer.

Either way, the return value is a function which can be called
anywhere to stop this spinner.  You can also call `spinner-stop'
in the same buffer where the spinner was created.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

DELAY, if given, is the number of seconds to wait until actually
displaying the spinner. It is safe to cancel the spinner before
this time, in which case it won't display at all.

\(fn &optional TYPE-OR-OBJECT FPS DELAY)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "spinner" '("spinner")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; spinner-autoloads.el ends here
;;; ssass-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ssass-mode" "ssass-mode.el" (0 0 0 0))
;;; Generated autoloads from ssass-mode.el

(autoload 'ssass-mode "ssass-mode" "\
Major mode for Sass

\(fn)" t nil)

(register-definition-prefixes "ssass-mode" '("ssass-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ssass-mode-autoloads.el ends here
;;; super-save-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "super-save" "super-save.el" (0 0 0 0))
;;; Generated autoloads from super-save.el

(defvar super-save-mode nil "\
Non-nil if Super-Save mode is enabled.
See the `super-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `super-save-mode'.")

(custom-autoload 'super-save-mode "super-save" nil)

(autoload 'super-save-mode "super-save" "\
A minor mode that saves your Emacs buffers when they lose focus.

If called interactively, enable Super-Save mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "super-save" '("super-save-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; super-save-autoloads.el ends here
;;; swiper-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "swiper" "swiper.el" (0 0 0 0))
;;; Generated autoloads from swiper.el

(autoload 'swiper-avy "swiper" "\
Jump to one of the current swiper candidates." t nil)

(autoload 'swiper-backward "swiper" "\
`isearch-backward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'swiper-thing-at-point "swiper" "\
`swiper' with `ivy-thing-at-point'." t nil)

(autoload 'swiper-all-thing-at-point "swiper" "\
`swiper-all' with `ivy-thing-at-point'." t nil)

(autoload 'swiper "swiper" "\
`isearch-forward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'swiper-all "swiper" "\
Run `swiper' for all open buffers.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'swiper-isearch "swiper" "\
A `swiper' that's not line-based.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'swiper-isearch-backward "swiper" "\
Like `swiper-isearch' but the first result is before the point.

\(fn &optional INITIAL-INPUT)" t nil)

(register-definition-prefixes "swiper" '("swiper-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; swiper-autoloads.el ends here
;;; tablist-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tablist" "tablist.el" (0 0 0 0))
;;; Generated autoloads from tablist.el

(autoload 'tablist-minor-mode "tablist" "\
Toggle Tablist minor mode on or off.

If called interactively, toggle `Tablist minor mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{tablist-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'tablist-mode "tablist" "\


\(fn)" t nil)

(register-definition-prefixes "tablist" '("tablist-"))

;;;***

;;;### (autoloads nil "tablist-filter" "tablist-filter.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from tablist-filter.el

(register-definition-prefixes "tablist-filter" '("tablist-filter-"))

;;;***

;;;### (autoloads nil nil ("tablist-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tablist-autoloads.el ends here
;;; toc-org-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "toc-org" "toc-org.el" (0 0 0 0))
;;; Generated autoloads from toc-org.el

(autoload 'toc-org-enable "toc-org" "\
Enable toc-org in this buffer." nil nil)

(autoload 'toc-org-mode "toc-org" "\
Toggle `toc-org' in this buffer.

If called interactively, toggle `Toc-Org mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "toc-org" '("toc-org-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; toc-org-autoloads.el ends here
;;; transient-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "transient" "transient.el" (0 0 0 0))
;;; Generated autoloads from transient.el

(autoload 'transient-insert-suffix "transient" "\
Insert a SUFFIX into PREFIX before LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

\(fn PREFIX LOC SUFFIX)" nil nil)

(function-put 'transient-insert-suffix 'lisp-indent-function 'defun)

(autoload 'transient-append-suffix "transient" "\
Insert a SUFFIX into PREFIX after LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

\(fn PREFIX LOC SUFFIX)" nil nil)

(function-put 'transient-append-suffix 'lisp-indent-function 'defun)

(autoload 'transient-replace-suffix "transient" "\
Replace the suffix at LOC in PREFIX with SUFFIX.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

\(fn PREFIX LOC SUFFIX)" nil nil)

(function-put 'transient-replace-suffix 'lisp-indent-function 'defun)

(autoload 'transient-remove-suffix "transient" "\
Remove the suffix or group at LOC in PREFIX.
PREFIX is a prefix command, a symbol.
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

\(fn PREFIX LOC)" nil nil)

(function-put 'transient-remove-suffix 'lisp-indent-function 'defun)

(register-definition-prefixes "transient" '("transient-"))

;;;***

;;;### (autoloads nil nil ("transient-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; transient-autoloads.el ends here
;;; tree-sitter-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tree-sitter" "tree-sitter.el" (0 0 0 0))
;;; Generated autoloads from tree-sitter.el

(autoload 'tree-sitter-mode "tree-sitter" "\
Minor mode that keeps an up-to-date syntax tree using incremental parsing.

If called interactively, toggle `Tree-Sitter mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-tree-sitter-mode "tree-sitter" "\
Turn on `tree-sitter-mode' in a buffer, if possible." nil nil)

(put 'global-tree-sitter-mode 'globalized-minor-mode t)

(defvar global-tree-sitter-mode nil "\
Non-nil if Global Tree-Sitter mode is enabled.
See the `global-tree-sitter-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-tree-sitter-mode'.")

(custom-autoload 'global-tree-sitter-mode "tree-sitter" nil)

(autoload 'global-tree-sitter-mode "tree-sitter" "\
Toggle Tree-Sitter mode in all buffers.
With prefix ARG, enable Global Tree-Sitter mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG is
omitted or nil.

Tree-Sitter mode is enabled in all buffers where
`turn-on-tree-sitter-mode' would do it.

See `tree-sitter-mode' for more information on Tree-Sitter mode.

\(fn &optional ARG)" t nil)

(autoload 'tree-sitter-node-at-point "tree-sitter" "\
Return the smallest syntax node at point whose type is NODE-TYPE.
If NODE-TYPE is nil, return the smallest syntax node at point.

\(fn &optional NODE-TYPE)" nil nil)

(register-definition-prefixes "tree-sitter" '("tree-sitter-"))

;;;***

;;;### (autoloads nil "tree-sitter-cli" "tree-sitter-cli.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from tree-sitter-cli.el

(register-definition-prefixes "tree-sitter-cli" '("tree-sitter-cli-"))

;;;***

;;;### (autoloads nil "tree-sitter-debug" "tree-sitter-debug.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from tree-sitter-debug.el

(autoload 'tree-sitter-debug-mode "tree-sitter-debug" "\
Toggle syntax tree debugging for the current buffer.
This mode displays the syntax tree in another buffer, and keeps it up-to-date.

If called interactively, toggle `Tree-Sitter-Debug mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'tree-sitter-debug-query "tree-sitter-debug" "\
Execute query PATTERNS against the current syntax tree and return captures.

If the optional arg MATCHES is non-nil, matches (from `tsc-query-matches') are
returned instead of captures (from `tsc-query-captures').

If the optional arg TAG-ASSIGNER is non-nil, it is passed to `tsc-make-query' to
assign custom tags to capture names.

This function is primarily useful for debugging purpose. Other packages should
build queries and cursors once, then reuse them.

\(fn PATTERNS &optional MATCHES TAG-ASSIGNER)" nil nil)

(register-definition-prefixes "tree-sitter-debug" '("tree-sitter-debug-"))

;;;***

;;;### (autoloads nil "tree-sitter-extras" "tree-sitter-extras.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from tree-sitter-extras.el

(autoload 'tree-sitter-save-excursion "tree-sitter-extras" "\
Save the current location within the syntax tree; execute BODY; restore it.

If the original location cannot be restored due to the syntax tree changing too
much, this macro behaves like `save-excursion', unless
`tree-sitter-save-excursion-try-hard' is non-nil, in which case it tries to get
as close as possible to the original location.

After the location is restored, the buffer text is scrolled so that point stays
at roughly the same vertical screen position. If `pixel-scroll' is available and
`tree-sitter-save-excursion-pixelwise' is non-nil, pixelwise scrolling is used
instead, to make this restoration exact.

\(fn &rest BODY)" nil t)

(function-put 'tree-sitter-save-excursion 'lisp-indent-function '0)

(register-definition-prefixes "tree-sitter-extras" '("tree-sitter-"))

;;;***

;;;### (autoloads nil "tree-sitter-hl" "tree-sitter-hl.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from tree-sitter-hl.el

(autoload 'tree-sitter-hl-add-patterns "tree-sitter-hl" "\
Add custom syntax highlighting PATTERNS.
If LANG-SYMBOL is non-nil, it identifies the language that PATTERNS should be
applied to. If LANG-SYMBOL is nil, PATTERNS are applied to the current buffer,
and are prioritized over language-specific patterns. Either way, PATTERNS are
prioritized over `tree-sitter-hl-default-patterns'.

This function should be used by minor modes and configuration code. Major modes
should set `tree-sitter-hl-default-patterns' instead.

\(fn LANG-SYMBOL PATTERNS)" nil nil)

(function-put 'tree-sitter-hl-add-patterns 'lisp-indent-function '1)

(autoload 'tree-sitter-hl-mode "tree-sitter-hl" "\
Toggle syntax highlighting based on Tree-sitter's syntax tree.
If `tree-sitter-hl-default-patterns' is nil, turning on this mode does nothing,
and does not interfere with `font-lock-mode'.

If called interactively, toggle `Tree-Sitter-Hl mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Enabling this automatically enables `tree-sitter-mode' in the buffer.

To enable this automatically whenever `tree-sitter-mode' is enabled:

 (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

\(fn &optional ARG)" t nil)

(register-definition-prefixes "tree-sitter-hl" '("tree-sitter-hl-"))

;;;***

;;;### (autoloads nil "tree-sitter-load" "tree-sitter-load.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from tree-sitter-load.el

(autoload 'tree-sitter-require "tree-sitter-load" "\
Return the language object loaded and registered under the name LANG-SYMBOL.
If the language has not been loaded yet, load it with `tree-sitter-load'.

FILE should be the base name (without extension) of the native shared library
that exports the language as the native symbol NATIVE-SYMBOL-NAME.

If FILE is nil, the base name is assumed to be LANG-SYMBOL's name.

If NATIVE-SYMBOL-NAME is nil, the name of the exported native symbol is assumed
to be LANG-SYMBOL's name, prefixed with \"tree_sitter_\".

\(fn LANG-SYMBOL &optional FILE NATIVE-SYMBOL-NAME)" nil nil)

(register-definition-prefixes "tree-sitter-load" '("tree-sitter-l"))

;;;***

;;;### (autoloads nil "tree-sitter-query" "tree-sitter-query.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from tree-sitter-query.el

(autoload 'tree-sitter-query-builder "tree-sitter-query" "\
Provide means for developers to write and test tree-sitter queries.

The buffer on focus when the command is called is set as the target buffer." t nil)

(register-definition-prefixes "tree-sitter-query" '("tree-sitter-query-"))

;;;***

;;;### (autoloads nil nil ("tree-sitter-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tree-sitter-autoloads.el ends here
;;; tree-sitter-indent-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tree-sitter-indent" "tree-sitter-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from tree-sitter-indent.el

(autoload 'tree-sitter-indent-line "tree-sitter-indent" "\
Use Tree-sitter as backend to indent current line." nil nil)

(register-definition-prefixes "tree-sitter-indent" '("tree-sitter-indent-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tree-sitter-indent-autoloads.el ends here
;;; tree-sitter-langs-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tree-sitter-langs" "tree-sitter-langs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from tree-sitter-langs.el

(register-definition-prefixes "tree-sitter-langs" '("tree-sitter-langs-"))

;;;***

;;;### (autoloads nil "tree-sitter-langs-build" "tree-sitter-langs-build.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from tree-sitter-langs-build.el

(autoload 'tree-sitter-langs-install-grammars "tree-sitter-langs-build" "\
Download and install the specified VERSION of the language grammar bundle.
If VERSION or OS is not specified, use the default of
`tree-sitter-langs--bundle-version' and `tree-sitter-langs--os'.

This installs the grammar bundle even if the same version was already installed,
unless SKIP-IF-INSTALLED is non-nil.

The download bundle file is deleted after installation, unless KEEP-BUNDLE is
non-nil.

\(fn &optional SKIP-IF-INSTALLED VERSION OS KEEP-BUNDLE)" t nil)

(register-definition-prefixes "tree-sitter-langs-build" '("tree-sitter-langs-"))

;;;***

;;;### (autoloads nil nil ("tree-sitter-langs-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tree-sitter-langs-autoloads.el ends here
;;; treemacs-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "treemacs" "treemacs.el" (0 0 0 0))
;;; Generated autoloads from treemacs.el

(autoload 'treemacs-version "treemacs" "\
Return the `treemacs-version'." t nil)

(autoload 'treemacs "treemacs" "\
Initialise or toggle treemacs.
* If the treemacs window is visible hide it.
* If a treemacs buffer exists, but is not visible show it.
* If no treemacs buffer exists for the current frame create and show it.
* If the workspace is empty additionally ask for the root path of the first
  project to add." t nil)

(autoload 'treemacs-find-file "treemacs" "\
Find and focus the current file in the treemacs window.
If the current buffer has visits no file or with a prefix ARG ask for the
file instead.
Will show/create a treemacs buffers if it is not visible/does not exist.
For the most part only useful when `treemacs-follow-mode' is not active.

\(fn &optional ARG)" t nil)

(autoload 'treemacs-find-tag "treemacs" "\
Find and move point to the tag at point in the treemacs view.
Most likely to be useful when `treemacs-tag-follow-mode' is not active.

Will ask to change the treemacs root if the file to find is not under the
root.  If no treemacs buffer exists it will be created with the current file's
containing directory as root.  Will do nothing if the current buffer is not
visiting a file or Emacs cannot find any tags for the current file." t nil)

(autoload 'treemacs-select-window "treemacs" "\
Select the treemacs window if it is visible.
Bring it to the foreground if it is not visible.
Initialise a new treemacs buffer as calling `treemacs' would if there is no
treemacs buffer for this frame." t nil)

(autoload 'treemacs-show-changelog "treemacs" "\
Show the changelog of treemacs." t nil)

(autoload 'treemacs-edit-workspaces "treemacs" "\
Edit your treemacs workspaces and projects as an `org-mode' file." t nil)

(autoload 'treemacs-display-current-project-exclusively "treemacs" "\
Display the current project, and *only* the current project.
Like `treemacs-add-and-display-current-project' this will add the current
project to treemacs based on either projectile or the built-in project.el.
However the 'exclusive' part means that it will make the current project the
only project, all other projects *will be removed* from the current workspace." t nil)

(autoload 'treemacs-add-and-display-current-project "treemacs" "\
Open treemacs and add the current project root to the workspace.
The project is determined first by projectile (if treemacs-projectile is
installed), then by project.el.
If the project is already registered with treemacs just move point to its root.
An error message is displayed if the current buffer is not part of any project." t nil)

(register-definition-prefixes "treemacs" '("treemacs-version"))

;;;***

;;;### (autoloads nil "treemacs-async" "treemacs-async.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from treemacs-async.el

(register-definition-prefixes "treemacs-async" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-bookmarks" "treemacs-bookmarks.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-bookmarks.el

(autoload 'treemacs-bookmark "treemacs-bookmarks" "\
Find a bookmark in treemacs.
Only bookmarks marking either a file or a directory are offered for selection.
Treemacs will try to find and focus the given bookmark's location, in a similar
fashion to `treemacs-find-file'.

With a prefix argument ARG treemacs will also open the bookmarked location.

\(fn &optional ARG)" t nil)

(autoload 'treemacs--bookmark-handler "treemacs-bookmarks" "\
Open Treemacs into a bookmark RECORD.

\(fn RECORD)" nil nil)

(autoload 'treemacs-add-bookmark "treemacs-bookmarks" "\
Add the current node to Emacs' list of bookmarks.
For file and directory nodes their absolute path is saved.  Tag nodes
additionally also save the tag's position.  A tag can only be bookmarked if the
treemacs node is pointing to a valid buffer position." t nil)

(register-definition-prefixes "treemacs-bookmarks" '("treemacs--"))

;;;***

;;;### (autoloads nil "treemacs-compatibility" "treemacs-compatibility.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-compatibility.el

(register-definition-prefixes "treemacs-compatibility" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-core-utils" "treemacs-core-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-core-utils.el

(register-definition-prefixes "treemacs-core-utils" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-customization" "treemacs-customization.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-customization.el

(register-definition-prefixes "treemacs-customization" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-diagnostics" "treemacs-diagnostics.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-diagnostics.el

(register-definition-prefixes "treemacs-diagnostics" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-dom" "treemacs-dom.el" (0 0 0 0))
;;; Generated autoloads from treemacs-dom.el

(register-definition-prefixes "treemacs-dom" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-extensions" "treemacs-extensions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-extensions.el

(register-definition-prefixes "treemacs-extensions" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-filewatch-mode" "treemacs-filewatch-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-filewatch-mode.el

(register-definition-prefixes "treemacs-filewatch-mode" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-follow-mode" "treemacs-follow-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-follow-mode.el

(register-definition-prefixes "treemacs-follow-mode" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-fringe-indicator" "treemacs-fringe-indicator.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-fringe-indicator.el

(register-definition-prefixes "treemacs-fringe-indicator" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-header-line" "treemacs-header-line.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-header-line.el

(register-definition-prefixes "treemacs-header-line" '("treemacs-header-buttons-format"))

;;;***

;;;### (autoloads nil "treemacs-hydras" "treemacs-hydras.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from treemacs-hydras.el

(autoload 'treemacs-common-helpful-hydra "treemacs-hydras" "\
Summon a helpful hydra to show you the treemacs keymap.

This hydra will show the most commonly used keybinds for treemacs.  For the more
advanced (probably rarely used keybinds) see `treemacs-advanced-helpful-hydra'.

The keybinds shown in this hydra are not static, but reflect the actual
keybindings currently in use (including evil mode).  If the hydra is unable to
find the key a command is bound to it will show a blank instead." t nil)

(autoload 'treemacs-advanced-helpful-hydra "treemacs-hydras" "\
Summon a helpful hydra to show you the treemacs keymap.

This hydra will show the more advanced (rarely used) keybinds for treemacs.  For
the more commonly used keybinds see `treemacs-common-helpful-hydra'.

The keybinds shown in this hydra are not static, but reflect the actual
keybindings currently in use (including evil mode).  If the hydra is unable to
find the key a command is bound to it will show a blank instead." t nil)

(register-definition-prefixes "treemacs-hydras" '("treemacs-helpful-hydra"))

;;;***

;;;### (autoloads nil "treemacs-icons" "treemacs-icons.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from treemacs-icons.el

(autoload 'treemacs-resize-icons "treemacs-icons" "\
Resize the current theme's icons to the given SIZE.

If SIZE is 'nil' the icons are not resized and will retain their default size of
22 pixels.

There is only one size, the icons are square and the aspect ratio will be
preserved when resizing them therefore width and height are the same.

Resizing the icons only works if Emacs was built with ImageMagick support, or if
using Emacs >= 27.1,which has native image resizing support.  If this is not the
case this function will not have any effect.

Custom icons are not taken into account, only the size of treemacs' own icons
png are changed.

\(fn SIZE)" t nil)

(autoload 'treemacs-define-custom-icon "treemacs-icons" "\
Define a custom ICON for the current theme to use for FILE-EXTENSIONS.

Note that treemacs has a very loose definition of what constitutes a file
extension - it's either everything past the last period, or just the file's full
name if there is no period.  This makes it possible to match file names like
'.gitignore' and 'Makefile'.

Additionally FILE-EXTENSIONS are also not case sensitive and will be stored in a
down-cased state.

\(fn ICON &rest FILE-EXTENSIONS)" nil nil)

(autoload 'treemacs-define-custom-image-icon "treemacs-icons" "\
Same as `treemacs-define-custom-icon' but for image icons instead of strings.
FILE is the path to an icon image (and not the actual icon string).
FILE-EXTENSIONS are all the (not case-sensitive) file extensions the icon
should be used for.

\(fn FILE &rest FILE-EXTENSIONS)" nil nil)

(autoload 'treemacs-map-icons-with-auto-mode-alist "treemacs-icons" "\
Remaps icons for EXTENSIONS according to `auto-mode-alist'.
EXTENSIONS should be a list of file extensions such that they match the regex
stored in `auto-mode-alist', for example '(\".cc\").
MODE-ICON-ALIST is an alist that maps which mode from `auto-mode-alist' should
be assigned which treemacs icon, for example
'((c-mode . treemacs-icon-c)
  (c++-mode . treemacs-icon-cpp))

\(fn EXTENSIONS MODE-ICON-ALIST)" nil nil)

(register-definition-prefixes "treemacs-icons" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-interface" "treemacs-interface.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-interface.el

(register-definition-prefixes "treemacs-interface" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-logging" "treemacs-logging.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from treemacs-logging.el

(register-definition-prefixes "treemacs-logging" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-macros" "treemacs-macros.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from treemacs-macros.el

(register-definition-prefixes "treemacs-macros" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-mode" "treemacs-mode.el" (0 0 0 0))
;;; Generated autoloads from treemacs-mode.el

(autoload 'treemacs-mode "treemacs-mode" "\
A major mode for displaying the file system in a tree layout.

\(fn)" t nil)

(register-definition-prefixes "treemacs-mode" '("treemacs--"))

;;;***

;;;### (autoloads nil "treemacs-mouse-interface" "treemacs-mouse-interface.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-mouse-interface.el

(autoload 'treemacs-leftclick-action "treemacs-mouse-interface" "\
Move focus to the clicked line.
Must be bound to a mouse click, or EVENT will not be supplied.

\(fn EVENT)" t nil)

(autoload 'treemacs-doubleclick-action "treemacs-mouse-interface" "\
Run the appropriate double-click action for the current node.
In the default configuration this means to do the same as `treemacs-RET-action'.

This function's exact configuration is stored in
`treemacs-doubleclick-actions-config'.

Must be bound to a mouse click, or EVENT will not be supplied.

\(fn EVENT)" t nil)

(autoload 'treemacs-single-click-expand-action "treemacs-mouse-interface" "\
A modified single-leftclick action that expands the clicked nodes.
Can be bound to <mouse1> if you prefer to expand nodes with a single click
instead of a double click.  Either way it must be bound to a mouse click, or
EVENT will not be supplied.

Clicking on icons will expand a file's tags, just like
`treemacs-leftclick-action'.

\(fn EVENT)" t nil)

(autoload 'treemacs-dragleftclick-action "treemacs-mouse-interface" "\
Drag a file/dir node to be opened in a window.
Must be bound to a mouse click, or EVENT will not be supplied.

\(fn EVENT)" t nil)

(autoload 'treemacs-define-doubleclick-action "treemacs-mouse-interface" "\
Define the behaviour of `treemacs-doubleclick-action'.
Determines that a button with a given STATE should lead to the execution of
ACTION.

The list of possible states can be found in `treemacs-valid-button-states'.
ACTION should be one of the `treemacs-visit-node-*' commands.

\(fn STATE ACTION)" nil nil)

(autoload 'treemacs-node-buffer-and-position "treemacs-mouse-interface" "\
Return source buffer or list of buffer and position for the current node.
This information can be used for future display.  Stay in the selected window
and ignore any prefix argument.

\(fn &optional _)" t nil)

(autoload 'treemacs-rightclick-menu "treemacs-mouse-interface" "\
Show a contextual right click menu based on click EVENT.

\(fn EVENT)" t nil)

(register-definition-prefixes "treemacs-mouse-interface" '("treemacs--"))

;;;***

;;;### (autoloads nil "treemacs-persistence" "treemacs-persistence.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-persistence.el

(register-definition-prefixes "treemacs-persistence" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-rendering" "treemacs-rendering.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-rendering.el

(register-definition-prefixes "treemacs-rendering" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-scope" "treemacs-scope.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from treemacs-scope.el

(register-definition-prefixes "treemacs-scope" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-tag-follow-mode" "treemacs-tag-follow-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-tag-follow-mode.el

(defvar treemacs-tag-follow-mode nil "\
Non-nil if Treemacs-Tag-Follow mode is enabled.
See the `treemacs-tag-follow-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-tag-follow-mode'.")

(custom-autoload 'treemacs-tag-follow-mode "treemacs-tag-follow-mode" nil)

(autoload 'treemacs-tag-follow-mode "treemacs-tag-follow-mode" "\
Toggle `treemacs-tag-follow-mode'.

If called interactively, toggle `Treemacs-Tag-Follow mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This acts as more fine-grained alternative to `treemacs-follow-mode' and will
thus disable `treemacs-follow-mode' on activation. When enabled treemacs will
focus not only the file of the current buffer, but also the tag at point.

The follow action is attached to Emacs' idle timer and will run
`treemacs-tag-follow-delay' seconds of idle time. The delay value is not an
integer, meaning it accepts floating point values like 1.5.

Every time a tag is followed a rescan of the imenu index is forced by
temporarily setting `imenu-auto-rescan' to t (though a cache is applied as long
as the buffer is unmodified). This is necessary to assure that creation or
deletion of tags does not lead to errors and guarantees an always up-to-date tag
view.

Note that in order to move to a tag in treemacs the treemacs buffer's window
needs to be temporarily selected, which will reset `blink-cursor-mode's timer if
it is enabled. This will result in the cursor blinking seemingly pausing for a
short time and giving the appereance of the tag follow action lasting much
longer than it really does.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "treemacs-tag-follow-mode" '("treemacs--"))

;;;***

;;;### (autoloads nil "treemacs-tags" "treemacs-tags.el" (0 0 0 0))
;;; Generated autoloads from treemacs-tags.el

(autoload 'treemacs--expand-file-node "treemacs-tags" "\
Open tag items for file BTN.
Recursively open all tags below BTN when RECURSIVE is non-nil.

\(fn BTN &optional RECURSIVE)" nil nil)

(autoload 'treemacs--collapse-file-node "treemacs-tags" "\
Close node given by BTN.
Remove all open tag entries under BTN when RECURSIVE.

\(fn BTN &optional RECURSIVE)" nil nil)

(autoload 'treemacs--visit-or-expand/collapse-tag-node "treemacs-tags" "\
Visit tag section BTN if possible, expand or collapse it otherwise.
Pass prefix ARG on to either visit or toggle action.

FIND-WINDOW is a special provision depending on this function's invocation
context and decides whether to find the window to display in (if the tag is
visited instead of the node being expanded).

On the one hand it can be called based on `treemacs-RET-actions-config' (or
TAB).  The functions in these configs are expected to find the windows they need
to display in themselves, so FIND-WINDOW must be t. On the other hand this
function is also called from the top level vist-node functions like
`treemacs-visit-node-vertical-split' which delegates to the
`treemacs--execute-button-action' macro which includes the determination of
the display window.

\(fn BTN ARG FIND-WINDOW)" nil nil)

(autoload 'treemacs--expand-tag-node "treemacs-tags" "\
Open tags node items for BTN.
Open all tag section under BTN when call is RECURSIVE.

\(fn BTN &optional RECURSIVE)" nil nil)

(autoload 'treemacs--collapse-tag-node "treemacs-tags" "\
Close tags node at BTN.
Remove all open tag entries under BTN when RECURSIVE.

\(fn BTN &optional RECURSIVE)" nil nil)

(autoload 'treemacs--goto-tag "treemacs-tags" "\
Go to the tag at BTN.

\(fn BTN)" nil nil)

(autoload 'treemacs--create-imenu-index-function "treemacs-tags" "\
The `imenu-create-index-function' for treemacs buffers." nil nil)

(function-put 'treemacs--create-imenu-index-function 'side-effect-free 't)

(register-definition-prefixes "treemacs-tags" '("treemacs--"))

;;;***

;;;### (autoloads nil "treemacs-themes" "treemacs-themes.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from treemacs-themes.el

(register-definition-prefixes "treemacs-themes" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-visuals" "treemacs-visuals.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from treemacs-visuals.el

(register-definition-prefixes "treemacs-visuals" '("treemacs-"))

;;;***

;;;### (autoloads nil "treemacs-workspaces" "treemacs-workspaces.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-workspaces.el

(register-definition-prefixes "treemacs-workspaces" '("treemacs-"))

;;;***

;;;### (autoloads nil nil ("treemacs-faces.el" "treemacs-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; treemacs-autoloads.el ends here
;;; tsc-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tsc" "tsc.el" (0 0 0 0))
;;; Generated autoloads from tsc.el

(register-definition-prefixes "tsc" '("tsc-"))

;;;***

;;;### (autoloads nil "tsc-dyn-get" "tsc-dyn-get.el" (0 0 0 0))
;;; Generated autoloads from tsc-dyn-get.el

(register-definition-prefixes "tsc-dyn-get" '("tsc-"))

;;;***

;;;### (autoloads nil nil ("tsc-obsolete.el" "tsc-pkg.el") (0 0 0
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tsc-autoloads.el ends here
;;; typescript-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "typescript-mode" "typescript-mode.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from typescript-mode.el
(put 'typescript-indent-level 'safe-local-variable #'integerp)

(autoload 'typescript-mode "typescript-mode" "\
Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}

\(fn)" t nil)

(eval-after-load 'folding '(when (fboundp 'folding-add-to-marks-list) (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}")))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "typescript-mode" '("typescript-")))

;;;***

;;;### (autoloads nil "typescript-mode-test-utilities" "typescript-mode-test-utilities.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from typescript-mode-test-utilities.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "typescript-mode-test-utilities" '("font-lock-test" "get-face-at" "test-with-")))

;;;***

;;;### (autoloads nil nil ("typescript-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; typescript-mode-autoloads.el ends here
;;; undo-fu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "undo-fu" "undo-fu.el" (0 0 0 0))
;;; Generated autoloads from undo-fu.el

(autoload 'undo-fu-disable-checkpoint "undo-fu" "\
Remove the undo-fu checkpoint, making all future actions unconstrained.

This command is needed when `undo-fu-ignore-keyboard-quit' is t,
since in this case `keyboard-quit' cannot be used
to perform unconstrained undo/redo actions." t nil)

(autoload 'undo-fu-only-redo-all "undo-fu" "\
Redo all actions until the initial undo step.

wraps the `undo' function." t nil)

(autoload 'undo-fu-only-redo "undo-fu" "\
Redo an action until the initial undo action.

wraps the `undo' function.

Optional argument ARG The number of steps to redo.

\(fn &optional ARG)" t nil)

(autoload 'undo-fu-only-undo "undo-fu" "\
Undo the last action.

wraps the `undo-only' function.

Optional argument ARG the number of steps to undo.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "undo-fu" '("undo-fu-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; undo-fu-autoloads.el ends here
;;; use-package-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "use-package-bind-key" "use-package-bind-key.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from use-package-bind-key.el

(autoload 'use-package-autoload-keymap "use-package-bind-key" "\
Loads PACKAGE and then binds the key sequence used to invoke
this function to KEYMAP-SYMBOL. It then simulates pressing the
same key sequence a again, so that the next key pressed is routed
to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword. It
works by binding the given key sequence to an invocation of this
function for a particular keymap. The keymap is expected to be
defined by the package. In this way, loading the package is
deferred until the prefix key sequence is pressed.

\(fn KEYMAP-SYMBOL PACKAGE OVERRIDE)" nil nil)

(autoload 'use-package-normalize-binder "use-package-bind-key" "\


\(fn NAME KEYWORD ARGS)" nil nil)

(defalias 'use-package-normalize/:bind 'use-package-normalize-binder)

(defalias 'use-package-normalize/:bind* 'use-package-normalize-binder)

(defalias 'use-package-autoloads/:bind 'use-package-autoloads-mode)

(defalias 'use-package-autoloads/:bind* 'use-package-autoloads-mode)

(autoload 'use-package-handler/:bind "use-package-bind-key" "\


\(fn NAME KEYWORD ARGS REST STATE &optional BIND-MACRO)" nil nil)

(defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder)

(defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder)

(autoload 'use-package-handler/:bind-keymap "use-package-bind-key" "\


\(fn NAME KEYWORD ARGS REST STATE &optional OVERRIDE)" nil nil)

(autoload 'use-package-handler/:bind-keymap* "use-package-bind-key" "\


\(fn NAME KEYWORD ARG REST STATE)" nil nil)

(register-definition-prefixes "use-package-bind-key" '("use-package-handler/:bind*"))

;;;***

;;;### (autoloads nil "use-package-core" "use-package-core.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from use-package-core.el

(autoload 'use-package "use-package-core" "\
Declare an Emacs package by specifying a group of configuration options.

For full documentation, please see the README file that came with
this file.  Usage:

  (use-package package-name
     [:keyword [option]]...)

:init            Code to run before PACKAGE-NAME has been loaded.
:config          Code to run after PACKAGE-NAME has been loaded.  Note that
                 if loading is deferred for any reason, this code does not
                 execute until the lazy load has occurred.
:preface         Code to be run before everything except `:disabled'; this
                 can be used to define functions for use in `:if', or that
                 should be seen by the byte-compiler.

:mode            Form to be added to `auto-mode-alist'.
:magic           Form to be added to `magic-mode-alist'.
:magic-fallback  Form to be added to `magic-fallback-mode-alist'.
:interpreter     Form to be added to `interpreter-mode-alist'.

:commands        Define autoloads for commands that will be defined by the
                 package.  This is useful if the package is being lazily
                 loaded, and you wish to conditionally call functions in your
                 `:init' block that are defined in the package.
:hook            Specify hook(s) to attach this package to.

:bind            Bind keys, and define autoloads for the bound commands.
:bind*           Bind keys, and define autoloads for the bound commands,
                 *overriding all minor mode bindings*.
:bind-keymap     Bind a key prefix to an auto-loaded keymap defined in the
                 package.  This is like `:bind', but for keymaps.
:bind-keymap*    Like `:bind-keymap', but overrides all minor mode bindings

:defer           Defer loading of a package -- this is implied when using
                 `:commands', `:bind', `:bind*', `:mode', `:magic', `:hook',
                 `:magic-fallback', or `:interpreter'.  This can be an integer,
                 to force loading after N seconds of idle time, if the package
                 has not already been loaded.
:after           Delay the use-package declaration until after the named modules
                 have loaded. Once load, it will be as though the use-package
                 declaration (without `:after') had been seen at that moment.
:demand          Prevent the automatic deferred loading introduced by constructs
                 such as `:bind' (see `:defer' for the complete list).

:if EXPR         Initialize and load only if EXPR evaluates to a non-nil value.
:disabled        The package is ignored completely if this keyword is present.
:defines         Declare certain variables to silence the byte-compiler.
:functions       Declare certain functions to silence the byte-compiler.
:load-path       Add to the `load-path' before attempting to load the package.
:diminish        Support for diminish.el (if installed).
:delight         Support for delight.el (if installed).
:custom          Call `custom-set' or `set-default' with each variable
                 definition without modifying the Emacs `custom-file'.
                 (compare with `custom-set-variables').
:custom-face     Call `customize-set-faces' with each face definition.
:ensure          Loads the package using package.el if necessary.
:pin             Pin the package to an archive.

\(fn NAME &rest ARGS)" nil t)

(function-put 'use-package 'lisp-indent-function '1)

(register-definition-prefixes "use-package-core" '("use-package-"))

;;;***

;;;### (autoloads nil "use-package-delight" "use-package-delight.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from use-package-delight.el

(autoload 'use-package-normalize/:delight "use-package-delight" "\
Normalize arguments to delight.

\(fn NAME KEYWORD ARGS)" nil nil)

(autoload 'use-package-handler/:delight "use-package-delight" "\


\(fn NAME KEYWORD ARGS REST STATE)" nil nil)

(register-definition-prefixes "use-package-delight" '("use-package-normalize-delight"))

;;;***

;;;### (autoloads nil "use-package-diminish" "use-package-diminish.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from use-package-diminish.el

(autoload 'use-package-normalize/:diminish "use-package-diminish" "\


\(fn NAME KEYWORD ARGS)" nil nil)

(autoload 'use-package-handler/:diminish "use-package-diminish" "\


\(fn NAME KEYWORD ARG REST STATE)" nil nil)

(register-definition-prefixes "use-package-diminish" '("use-package-normalize-diminish"))

;;;***

;;;### (autoloads nil "use-package-ensure" "use-package-ensure.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from use-package-ensure.el

(autoload 'use-package-normalize/:ensure "use-package-ensure" "\


\(fn NAME KEYWORD ARGS)" nil nil)

(autoload 'use-package-handler/:ensure "use-package-ensure" "\


\(fn NAME KEYWORD ENSURE REST STATE)" nil nil)

(register-definition-prefixes "use-package-ensure" '("use-package-"))

;;;***

;;;### (autoloads nil "use-package-jump" "use-package-jump.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from use-package-jump.el

(autoload 'use-package-jump-to-package-form "use-package-jump" "\
Attempt to find and jump to the `use-package' form that loaded
PACKAGE. This will only find the form if that form actually
required PACKAGE. If PACKAGE was previously required then this
function will jump to the file that originally required PACKAGE
instead.

\(fn PACKAGE)" t nil)

(register-definition-prefixes "use-package-jump" '("use-package-find-require"))

;;;***

;;;### (autoloads nil "use-package-lint" "use-package-lint.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from use-package-lint.el

(autoload 'use-package-lint "use-package-lint" "\
Check for errors in use-package declarations.
For example, if the module's `:if' condition is met, but even
with the specified `:load-path' the module cannot be found." t nil)

(register-definition-prefixes "use-package-lint" '("use-package-lint-declaration"))

;;;***

;;;### (autoloads nil nil ("use-package-pkg.el" "use-package.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; use-package-autoloads.el ends here
;;; valign-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "valign" "valign.el" (0 0 0 0))
;;; Generated autoloads from valign.el

(autoload 'valign-table "valign" "\
Visually align the table at point." t nil)

(autoload 'valign-mode "valign" "\
Visually align Org tables.

If called interactively, toggle `Valign mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "valign" '("valign-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; valign-autoloads.el ends here
;;; vimrc-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vimrc-mode" "vimrc-mode.el" (0 0 0 0))
;;; Generated autoloads from vimrc-mode.el
 (add-to-list 'auto-mode-alist '("\\.vim\\'" . vimrc-mode))
 (add-to-list 'auto-mode-alist '("[._]?g?vimrc\\'" . vimrc-mode))
 (add-to-list 'auto-mode-alist '("\\.exrc\\'" . vimrc-mode))

(autoload 'vimrc-mode "vimrc-mode" "\
Major mode for editing `vimrc', `xxx.vim' and `.exrc' configuration files.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vimrc-mode" '("vimrc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vimrc-mode-autoloads.el ends here
;;; visual-fill-column-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "visual-fill-column" "visual-fill-column.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from visual-fill-column.el

(autoload 'visual-fill-column-mode "visual-fill-column" "\
Wrap lines according to `fill-column' in `visual-line-mode'.

If called interactively, toggle `Visual-Fill-Column mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-visual-fill-column-mode 'globalized-minor-mode t)

(defvar global-visual-fill-column-mode nil "\
Non-nil if Global Visual-Fill-Column mode is enabled.
See the `global-visual-fill-column-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-visual-fill-column-mode'.")

(custom-autoload 'global-visual-fill-column-mode "visual-fill-column" nil)

(autoload 'global-visual-fill-column-mode "visual-fill-column" "\
Toggle Visual-Fill-Column mode in all buffers.
With prefix ARG, enable Global Visual-Fill-Column mode if ARG is
positive; otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Visual-Fill-Column mode is enabled in all buffers where
`turn-on-visual-fill-column-mode' would do it.

See `visual-fill-column-mode' for more information on
Visual-Fill-Column mode.

\(fn &optional ARG)" t nil)

(autoload 'visual-fill-column-split-window-sensibly "visual-fill-column" "\
Split WINDOW sensibly, unsetting its margins first.
This function unsets the window margins and calls
`split-window-sensibly'.

By default, `split-window-sensibly' does not split a window in
two side-by-side windows if it has wide margins, even if there is
enough space for a vertical split.  This function is used as the
value of `split-window-preferred-function' to allow
`display-buffer' to split such windows.

\(fn &optional WINDOW)" nil nil)

(register-definition-prefixes "visual-fill-column" '("turn-on-visual-fill-column-mode" "visual-fill-column-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; visual-fill-column-autoloads.el ends here
;;; volatile-highlights-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "volatile-highlights" "volatile-highlights.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from volatile-highlights.el

(defvar volatile-highlights-mode nil "\
Non-nil if Volatile-Highlights mode is enabled.
See the `volatile-highlights-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `volatile-highlights-mode'.")

(custom-autoload 'volatile-highlights-mode "volatile-highlights" nil)

(autoload 'volatile-highlights-mode "volatile-highlights" "\
Minor mode for visual feedback on some operations.

If called interactively, enable Volatile-Highlights mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "volatile-highlights" '("Vhl/highlight-zero-width-ranges" "vhl/")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; volatile-highlights-autoloads.el ends here
;;; web-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "web-mode" "web-mode.el" (0 0 0 0))
;;; Generated autoloads from web-mode.el

(autoload 'web-mode "web-mode" "\
Major mode for editing web templates.

\(fn)" t nil)

(register-definition-prefixes "web-mode" '("web-mode-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; web-mode-autoloads.el ends here
;;; wgrep-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wgrep" "wgrep.el" (0 0 0 0))
;;; Generated autoloads from wgrep.el

(autoload 'wgrep-setup "wgrep" "\
Setup wgrep preparation." nil nil)

(add-hook 'grep-setup-hook 'wgrep-setup)

(register-definition-prefixes "wgrep" '("wgrep-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wgrep-autoloads.el ends here
;;; which-key-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "which-key" "which-key.el" (0 0 0 0))
;;; Generated autoloads from which-key.el

(defvar which-key-mode nil "\
Non-nil if Which-Key mode is enabled.
See the `which-key-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `which-key-mode'.")

(custom-autoload 'which-key-mode "which-key" nil)

(autoload 'which-key-mode "which-key" "\
Toggle which-key-mode.

If called interactively, toggle `Which-Key mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'which-key-setup-side-window-right "which-key" "\
Apply suggested settings for side-window that opens on right." t nil)

(autoload 'which-key-setup-side-window-right-bottom "which-key" "\
Apply suggested settings for side-window that opens on right
if there is space and the bottom otherwise." t nil)

(autoload 'which-key-setup-side-window-bottom "which-key" "\
Apply suggested settings for side-window that opens on
bottom." t nil)

(autoload 'which-key-setup-minibuffer "which-key" "\
Apply suggested settings for minibuffer.
Do not use this setup if you use the paging commands. Instead use
`which-key-setup-side-window-bottom', which is nearly identical
but more functional." t nil)

(autoload 'which-key-add-keymap-based-replacements "which-key" "\
Replace the description of KEY using REPLACEMENT in KEYMAP.
KEY should take a format suitable for use in
`kbd'. REPLACEMENT is the string to use to describe the
command associated with KEY in the KEYMAP. You may also use a
cons cell of the form (STRING . COMMAND) for each REPLACEMENT,
where STRING is the replacement string and COMMAND is a symbol
corresponding to the intended command to be replaced. In the
latter case, which-key will verify the intended command before
performing the replacement. COMMAND should be nil if the binding
corresponds to a key prefix. For example,

\(which-key-add-keymap-based-replacements global-map
  \"C-x w\" \"Save as\")

and

\(which-key-add-keymap-based-replacements global-map
  \"C-x w\" '(\"Save as\" . write-file))

both have the same effect for the \"C-x C-w\" key binding, but
the latter causes which-key to verify that the key sequence is
actually bound to write-file before performing the replacement.

\(fn KEYMAP KEY REPLACEMENT &rest MORE)" nil nil)

(autoload 'which-key-add-key-based-replacements "which-key" "\
Replace the description of KEY-SEQUENCE with REPLACEMENT.
KEY-SEQUENCE is a string suitable for use in `kbd'. REPLACEMENT
may either be a string, as in

\(which-key-add-key-based-replacements \"C-x 1\" \"maximize\")

a cons of two strings as in

\(which-key-add-key-based-replacements \"C-x 8\"
                                        '(\"unicode\" . \"Unicode keys\"))

or a function that takes a (KEY . BINDING) cons and returns a
replacement.

In the second case, the second string is used to provide a longer
name for the keys under a prefix.

MORE allows you to specifcy additional KEY REPLACEMENT pairs.  All
replacements are added to `which-key-replacement-alist'.

\(fn KEY-SEQUENCE REPLACEMENT &rest MORE)" nil nil)

(autoload 'which-key-add-major-mode-key-based-replacements "which-key" "\
Functions like `which-key-add-key-based-replacements'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and REPLACEMENT (MORE contains
addition KEY-SEQUENCE REPLACEMENT pairs) to apply.

\(fn MODE KEY-SEQUENCE REPLACEMENT &rest MORE)" nil nil)

(autoload 'which-key-reload-key-sequence "which-key" "\
Simulate entering the key sequence KEY-SEQ.
KEY-SEQ should be a list of events as produced by
`listify-key-sequence'. If nil, KEY-SEQ defaults to
`which-key--current-key-list'. Any prefix arguments that were
used are reapplied to the new key sequence.

\(fn &optional KEY-SEQ)" nil nil)

(autoload 'which-key-show-standard-help "which-key" "\
Call the command in `which-key--prefix-help-cmd-backup'.
Usually this is `describe-prefix-bindings'.

\(fn &optional _)" t nil)

(autoload 'which-key-show-next-page-no-cycle "which-key" "\
Show next page of keys unless on the last page, in which case
call `which-key-show-standard-help'." t nil)

(autoload 'which-key-show-previous-page-no-cycle "which-key" "\
Show previous page of keys unless on the first page, in which
case do nothing." t nil)

(autoload 'which-key-show-next-page-cycle "which-key" "\
Show the next page of keys, cycling from end to beginning
after last page.

\(fn &optional _)" t nil)

(autoload 'which-key-show-previous-page-cycle "which-key" "\
Show the previous page of keys, cycling from beginning to end
after first page.

\(fn &optional _)" t nil)

(autoload 'which-key-show-top-level "which-key" "\
Show top-level bindings.

\(fn &optional _)" t nil)

(autoload 'which-key-show-major-mode "which-key" "\
Show top-level bindings in the map of the current major mode.

This function will also detect evil bindings made using
`evil-define-key' in this map. These bindings will depend on the
current evil state. 

\(fn &optional ALL)" t nil)

(autoload 'which-key-show-full-major-mode "which-key" "\
Show all bindings in the map of the current major mode.

This function will also detect evil bindings made using
`evil-define-key' in this map. These bindings will depend on the
current evil state. " t nil)

(autoload 'which-key-dump-bindings "which-key" "\
Dump bindings from PREFIX into buffer named BUFFER-NAME.

PREFIX should be a string suitable for `kbd'.

\(fn PREFIX BUFFER-NAME)" t nil)

(autoload 'which-key-undo-key "which-key" "\
Undo last keypress and force which-key update.

\(fn &optional _)" t nil)

(autoload 'which-key-C-h-dispatch "which-key" "\
Dispatch C-h commands by looking up key in
`which-key-C-h-map'. This command is always accessible (from any
prefix) if `which-key-use-C-h-commands' is non nil." t nil)

(autoload 'which-key-show-keymap "which-key" "\
Show the top-level bindings in KEYMAP using which-key. KEYMAP
is selected interactively from all available keymaps.

If NO-PAGING is non-nil, which-key will not intercept subsequent
keypresses for the paging functionality.

\(fn KEYMAP &optional NO-PAGING)" t nil)

(autoload 'which-key-show-full-keymap "which-key" "\
Show all bindings in KEYMAP using which-key. KEYMAP is
selected interactively from all available keymaps.

\(fn KEYMAP)" t nil)

(autoload 'which-key-show-minor-mode-keymap "which-key" "\
Show the top-level bindings in KEYMAP using which-key. KEYMAP
is selected interactively by mode in `minor-mode-map-alist'.

\(fn &optional ALL)" t nil)

(autoload 'which-key-show-full-minor-mode-keymap "which-key" "\
Show all bindings in KEYMAP using which-key. KEYMAP
is selected interactively by mode in `minor-mode-map-alist'." t nil)

(register-definition-prefixes "which-key" '("which-key-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; which-key-autoloads.el ends here
;;; with-editor-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "with-editor" "with-editor.el" (0 0 0 0))
;;; Generated autoloads from with-editor.el

(autoload 'with-editor-export-editor "with-editor" "\
Teach subsequent commands to use current Emacs instance as editor.

Set and export the environment variable ENVVAR, by default
\"EDITOR\".  The value is automatically generated to teach
commands to use the current Emacs instance as \"the editor\".

This works in `shell-mode', `term-mode', `eshell-mode' and
`vterm'.

\(fn &optional (ENVVAR \"EDITOR\"))" t nil)

(autoload 'with-editor-export-git-editor "with-editor" "\
Like `with-editor-export-editor' but always set `$GIT_EDITOR'." t nil)

(autoload 'with-editor-export-hg-editor "with-editor" "\
Like `with-editor-export-editor' but always set `$HG_EDITOR'." t nil)

(defvar shell-command-with-editor-mode nil "\
Non-nil if Shell-Command-With-Editor mode is enabled.
See the `shell-command-with-editor-mode' command
for a description of this minor mode.")

(custom-autoload 'shell-command-with-editor-mode "with-editor" nil)

(autoload 'shell-command-with-editor-mode "with-editor" "\
Teach `shell-command' to use current Emacs instance as editor.

If called interactively, toggle `Shell-Command-With-Editor mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Teach `shell-command', and all commands that ultimately call that
command, to use the current Emacs instance as editor by executing
\"EDITOR=CLIENT COMMAND&\" instead of just \"COMMAND&\".

CLIENT is automatically generated; EDITOR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming no other variable overrides the effect of \"$EDITOR\".
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Alternatively you can use the `with-editor-async-shell-command',
which also allows the use of another variable instead of
\"EDITOR\".

\(fn &optional ARG)" t nil)

(autoload 'with-editor-async-shell-command "with-editor" "\
Like `async-shell-command' but with `$EDITOR' set.

Execute string \"ENVVAR=CLIENT COMMAND\" in an inferior shell;
display output, if any.  With a prefix argument prompt for an
environment variable, otherwise the default \"EDITOR\" variable
is used.  With a negative prefix argument additionally insert
the COMMAND's output at point.

CLIENT is automatically generated; ENVVAR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming it respects ENVVAR as an \"EDITOR\"-like variable.
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Also see `async-shell-command' and `shell-command'.

\(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t nil)

(autoload 'with-editor-shell-command "with-editor" "\
Like `shell-command' or `with-editor-async-shell-command'.
If COMMAND ends with \"&\" behave like the latter,
else like the former.

\(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t nil)

(register-definition-prefixes "with-editor" '("server-" "shell-command--shell-command-with-editor-mode" "start-file-process--with-editor-process-filter" "with-editor"))

;;;***

;;;### (autoloads nil nil ("with-editor-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; with-editor-autoloads.el ends here
;;; writeroom-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "writeroom-mode" "writeroom-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from writeroom-mode.el

(autoload 'writeroom-mode "writeroom-mode" "\
Minor mode for distraction-free writing.

If called interactively, enable Writeroom mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-writeroom-mode 'globalized-minor-mode t)

(defvar global-writeroom-mode nil "\
Non-nil if Global Writeroom mode is enabled.
See the `global-writeroom-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-writeroom-mode'.")

(custom-autoload 'global-writeroom-mode "writeroom-mode" nil)

(autoload 'global-writeroom-mode "writeroom-mode" "\
Toggle Writeroom mode in all buffers.
With prefix ARG, enable Global Writeroom mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Writeroom mode is enabled in all buffers where
`turn-on-writeroom-mode' would do it.
See `writeroom-mode' for more information on Writeroom mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "writeroom-mode" '("alpha" "bottom-divider-width" "define-writeroom-global-effect" "fullscreen" "internal-border-width" "menu-bar-lines" "sticky" "tool-bar-lines" "turn-on-writeroom-mode" "vertical-scroll-bars" "writeroom-")))

;;;***

;;;### (autoloads nil nil ("writeroom-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; writeroom-mode-autoloads.el ends here
;;; wucuo-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wucuo" "wucuo.el" (0 0 0 0))
;;; Generated autoloads from wucuo.el

(autoload 'wucuo-register-extra-typo-detection-algorithms "wucuo" "\
Register extra typo detection algorithms." nil nil)

(autoload 'wucuo-current-font-face "wucuo" "\
Get font face under cursor.
If QUIET is t, font face is not output.

\(fn &optional QUIET)" t nil)

(autoload 'wucuo-split-camel-case "wucuo" "\
Split camel case WORD into a list of strings.
Ported from 'https://github.com/fatih/camelcase/blob/master/camelcase.go'.

\(fn WORD)" nil nil)

(autoload 'wucuo-check-camel-case-word-predicate "wucuo" "\
Use aspell to check WORD.  If it's typo return t.

\(fn WORD)" nil nil)

(autoload 'wucuo-typo-p "wucuo" "\
Spell check WORD and return t if it's typo.
This is slow because new shell process is created.

\(fn WORD)" nil nil)

(autoload 'wucuo-generic-check-word-predicate "wucuo" "\
Function providing per-mode customization over which words are spell checked.
Returns t to continue checking, nil otherwise." nil nil)

(autoload 'wucuo-create-aspell-personal-dictionary "wucuo" "\
Create aspell personal dictionary." t nil)

(autoload 'wucuo-create-hunspell-personal-dictionary "wucuo" "\
Create hunspell personal dictionary." t nil)

(autoload 'wucuo-version "wucuo" "\
Output version." nil nil)

(autoload 'wucuo-spell-check-visible-region "wucuo" "\
Spell check visible region in current buffer." t nil)

(autoload 'wucuo-spell-check-buffer "wucuo" "\
Spell check current buffer." nil nil)

(autoload 'wucuo-start "wucuo" "\
Turn on wucuo to spell check code.  ARG is ignored.

\(fn &optional ARG)" t nil)

(autoload 'wucuo-aspell-cli-args "wucuo" "\
Create arguments for aspell cli.
If RUN-TOGETHER is t, aspell can check camel cased word.

\(fn &optional RUN-TOGETHER)" nil nil)

(autoload 'wucuo-flyspell-highlight-incorrect-region-hack "wucuo" "\
Don't mark doublon (double words) as typo.  ORIG-FUNC and ARGS is part of advice.

\(fn ORIG-FUNC &rest ARGS)" nil nil)

(autoload 'wucuo-spell-check-file "wucuo" "\
Spell check FILE and report all typos.
If KILL-EMACS-P is t, kill the Emacs and set exit program code.
If FULL-PATH-P is t, always show typo's file full path.
Return t if there is typo.

\(fn FILE &optional KILL-EMACS-P FULL-PATH-P)" nil nil)

(autoload 'wucuo-find-file-predicate "wucuo" "\
True if FILE does match `wucuo-find-file-regexp'.
And FILE does not match `wucuo-exclude-file-regexp'.
DIR is the directory containing FILE.

\(fn FILE DIR)" nil nil)

(autoload 'wucuo-find-directory-predicate "wucuo" "\
True if DIR is not a dot file, and not a symlink.
And DIR does not match `wucuo-exclude-directories'.
PARENT is the parent directory of DIR.

\(fn DIR PARENT)" nil nil)

(autoload 'wucuo-spell-check-directory "wucuo" "\
Spell check DIRECTORY and report all typos.
If KILL-EMACS-P is t, kill the Emacs and set exit program code.
If FULL-PATH-P is t, always show typo's file full path.

\(fn DIRECTORY &optional KILL-EMACS-P FULL-PATH-P)" nil nil)

(register-definition-prefixes "wucuo" '("wucuo-"))

;;;***

;;;### (autoloads nil "wucuo-flyspell-html-verify" "wucuo-flyspell-html-verify.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from wucuo-flyspell-html-verify.el

(autoload 'wucuo-flyspell-html-verify "wucuo-flyspell-html-verify" "\
Verify typo in html and xml file." nil nil)

;;;***

;;;### (autoloads nil "wucuo-flyspell-org-verify" "wucuo-flyspell-org-verify.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from wucuo-flyspell-org-verify.el

(autoload 'wucuo-flyspell-org-verify "wucuo-flyspell-org-verify" "\
Verify typo in org file." nil nil)

(register-definition-prefixes "wucuo-flyspell-org-verify" '("wucuo-org-mode-code-snippet-p"))

;;;***

;;;### (autoloads nil "wucuo-sdk" "wucuo-sdk.el" (0 0 0 0))
;;; Generated autoloads from wucuo-sdk.el

(autoload 'wucuo-sdk-current-line "wucuo-sdk" "\
Current line." nil nil)

(autoload 'wucuo-sdk-get-font-face "wucuo-sdk" "\
Get font face at POSITION.

\(fn POSITION)" nil nil)

;;;***

;;;### (autoloads nil nil ("wucuo-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wucuo-autoloads.el ends here
;;; xclip-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "xclip" "xclip.el" (0 0 0 0))
;;; Generated autoloads from xclip.el

(defvar xclip-mode nil "\
Non-nil if Xclip mode is enabled.
See the `xclip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `xclip-mode'.")

(custom-autoload 'xclip-mode "xclip" nil)

(autoload 'xclip-mode "xclip" "\
Minor mode to use the `xclip' program to copy&paste.

If called interactively, enable Xclip mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xclip" '("xclip-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; xclip-autoloads.el ends here
;;; xr-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "xr" "xr.el" (0 0 0 0))
;;; Generated autoloads from xr.el

(autoload 'xr "xr" "\
Convert a regexp string to rx notation; the inverse of `rx'.
Passing the returned value to `rx' (or `rx-to-string') yields a regexp string
equivalent to RE-STRING.  DIALECT controls the choice of keywords,
and is one of:
`verbose'       -- verbose keywords
`brief'         -- short keywords
`terse'         -- very short keywords
`medium' or nil -- a compromise (the default)

\(fn RE-STRING &optional DIALECT)" nil nil)

(autoload 'xr-skip-set "xr" "\
Convert a skip set string argument to rx notation.
SKIP-SET-STRING is interpreted according to the syntax of
`skip-chars-forward' and `skip-chars-backward' and converted to
a character class on `rx' form.
If desired, `rx' can then be used to convert the result to an
ordinary regexp.
See `xr' for a description of the DIALECT argument.

\(fn SKIP-SET-STRING &optional DIALECT)" nil nil)

(autoload 'xr-lint "xr" "\
Detect dubious practices and possible mistakes in RE-STRING.
This includes uses of tolerated but discouraged constructs.
Outright regexp syntax violations are signalled as errors.
If PURPOSE is `file', perform additional checks assuming that RE-STRING
is used to match a file name.
Return a list of (OFFSET . COMMENT) where COMMENT applies at OFFSET
in RE-STRING.

\(fn RE-STRING &optional PURPOSE)" nil nil)

(autoload 'xr-skip-set-lint "xr" "\
Detect dubious practices and possible mistakes in SKIP-SET-STRING.
This includes uses of tolerated but discouraged constructs.
Outright syntax violations are signalled as errors.
The argument is interpreted according to the syntax of
`skip-chars-forward' and `skip-chars-backward'.
Return a list of (OFFSET . COMMENT) where COMMENT applies at OFFSET
in SKIP-SET-STRING.

\(fn SKIP-SET-STRING)" nil nil)

(autoload 'xr-pp "xr" "\
Convert to `rx' notation and output the pretty-printed result.
This function uses `xr' to translate RE-STRING into DIALECT.
It is intended for use from an interactive elisp session.
See `xr' for a description of the DIALECT argument.

\(fn RE-STRING &optional DIALECT)" nil nil)

(autoload 'xr-skip-set-pp "xr" "\
Convert a skip set string to `rx' notation and pretty-print.
This function uses `xr-skip-set' to translate SKIP-SET-STRING
into DIALECT.
It is intended for use from an interactive elisp session.
See `xr' for a description of the DIALECT argument.

\(fn SKIP-SET-STRING &optional DIALECT)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xr" '("xr-")))

;;;***

;;;### (autoloads nil nil ("xr-pkg.el" "xr-test.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; xr-autoloads.el ends here
;;; yaml-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yaml-mode" "yaml-mode.el" (0 0 0 0))
;;; Generated autoloads from yaml-mode.el

(let ((loads (get 'yaml 'custom-loads))) (if (member '"yaml-mode" loads) nil (put 'yaml 'custom-loads (cons '"yaml-mode" loads))))

(autoload 'yaml-mode "yaml-mode" "\
Simple mode to edit YAML.

\\{yaml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yaml-mode" '("yaml-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yaml-mode-autoloads.el ends here
;;; yapfify-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yapfify" "yapfify.el" (0 0 0 0))
;;; Generated autoloads from yapfify.el

(autoload 'yapfify-region "yapfify" "\
Try to yapfify the current region.

If yapf exits with an error, the output will be shown in a help-window.

\(fn BEGINNING END)" t nil)

(autoload 'yapfify-buffer "yapfify" "\
Yapfify whole buffer." t nil)

(autoload 'yapfify-region-or-buffer "yapfify" "\
Yapfify the region if it is active. Otherwise, yapfify the buffer" t nil)

(autoload 'yapf-mode "yapfify" "\
Automatically run YAPF before saving.

If called interactively, enable Yapf mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yapfify" '("get-buffer-string" "yapfify-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yapfify-autoloads.el ends here
;;; yasnippet-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yasnippet" "yasnippet.el" (0 0 0 0))
;;; Generated autoloads from yasnippet.el

(autoload 'yas-minor-mode "yasnippet" "\
Toggle YASnippet mode.

If called interactively, enable Yas minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

When YASnippet mode is enabled, `yas-expand', normally bound to
the TAB key, expands snippets of code depending on the major
mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

Key bindings:
\\{yas-minor-mode-map}

\(fn &optional ARG)" t nil)

(put 'yas-global-mode 'globalized-minor-mode t)

(defvar yas-global-mode nil "\
Non-nil if Yas-Global mode is enabled.
See the `yas-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `yas-global-mode'.")

(custom-autoload 'yas-global-mode "yasnippet" nil)

(autoload 'yas-global-mode "yasnippet" "\
Toggle Yas minor mode in all buffers.
With prefix ARG, enable Yas-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Yas minor mode is enabled in all buffers where
`yas-minor-mode-on' would do it.
See `yas-minor-mode' for more information on Yas minor mode.

\(fn &optional ARG)" t nil)
(autoload 'snippet-mode "yasnippet" "A mode for editing yasnippets" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yasnippet" '("help-snippet-def" "snippet-mode-map" "yas")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yasnippet-autoloads.el ends here
;;; yasnippet-snippets-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yasnippet-snippets" "yasnippet-snippets.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from yasnippet-snippets.el

(autoload 'yasnippet-snippets-initialize "yasnippet-snippets" "\
Load the `yasnippet-snippets' snippets directory." nil nil)

(eval-after-load 'yasnippet '(yasnippet-snippets-initialize))

(register-definition-prefixes "yasnippet-snippets" '("yasnippet-snippets-"))

;;;***

;;;### (autoloads nil nil ("yasnippet-snippets-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yasnippet-snippets-autoloads.el ends here
