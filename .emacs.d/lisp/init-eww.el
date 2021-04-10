(with-eval-after-load 'eww
  (custom-set-variables
   '(eww-search-prefix "https://www.bing.com/search?q=")
   )
  ;; (define-key eww-mode-map (kbd "<C-i>")    'eww-forward-url)
  )
(provide 'init-eww)
