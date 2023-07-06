;; :list placeholder describes list
(matchp '(:list) '((1 2 3)))

;; :symbol placeholder describes one symbol
(matchp '(:symbol) '(a))

;; :etc placeholder describes one and more symbols
(matchp '(:etc) '(a b c))

;; placeholders may be nested
(matchp '(:symbol (:symbol :list)) '(a (b (c d))))

;; you can mix placeholders and values
(matchp '(a :symbol (b :list c)) '(a / (b (1 2 3) c)))
