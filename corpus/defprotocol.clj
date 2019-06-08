(ns defprotocol)

(defprotocol Foo
  "This is my protocol"
  (-foo [this] [this x] [this x y] "foo docs"))

(extend-protocol Foo
  nil
  (-foo ([this]) ([this x]) (this x y)))

(-foo nil)
(-foo nil 1)
(-foo nil 1 2)
(-foo nil 1 2 3) ;; wrong
