(create-hash-table conformist-collection nil
                   (:symbol (symbolp t))
                   (:list (listp t))
                   (:symbols (symbolp nil))
                   (:lists (listp nil)))
