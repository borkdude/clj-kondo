# Analysis data

Clj-kondo can provide data that was collected during linting, which enables
writing tools and linters that are not yet in clj-kondo itself. To get this
data, use the following configuration:

``` shellsession
{:output {:analysis true}}
```

When using clj-kondo from the command line, the analysis data will be exported
with `{:output {:format ...}}` set to `:json` or `:edn`.

## Data

The analysis output consists of a map with:

- `:namespace-definitions`, a list of maps with:
  - `:filename`, `:row`, `:col`
  - `:name`: the name of the namespace

  Optional:
  - several metadata values: `:deprecated`, `:doc`, `:author`, `:added`, `:no-doc` (used by
    [codox](https://github.com/weavejester/codox)).

 - `:namespace-usages`, a list of maps with:
   - `:filename`, `:row`, `:col`
   - `:from`: the namespace which uses
   - `:to`: the used namespace

- `:var-definitions`, a list of maps with:
  - `:filename`, `:row`, `:col`
  - `:ns`: the namespace of the var
  - `:name`: the name of the var

  Optional:
  - `:fixed-arities`: a set of fixed arities
  - `:var-args-min-arity`: the minimal number of arguments of a var-args signature
  - several metadata values: `:private`, `:macro`, `:deprecated`, `:doc`, `:added`

- `:var-usages`, a list of maps with:
  - `:filename`, `:row`, `:col`
  - `:name`: the name of the used var
  - `:from`: the namespace from which the var was used
  - `:to`: the namespace of the used var

  Optional:
  - `:arity`: if the usage was a function call, the amount of arguments passed

Example output after linting this code:

``` clojure
(ns foo
  "This is a useful namespace."
  {:deprecated "1.3"
   :author "Michiel Borkent"
   :no-doc true}
  (:require [clojure.set]))

(defn- f [x]
  (inc x))

(defmacro g
  "No longer used."
  {:added "1.2"
   :deprecated "1.3"}
  [x & xs]
  `(comment ~x ~@xs))
```

``` clojure
$ clj -m clj-kondo.tools.pprint edn /tmp/foo.clj
{:namespace-definitions
 [{:filename "/tmp/foo.clj",
   :row 1,
   :col 1,
   :name foo,
   :deprecated "1.3",
   :doc "This is a useful namespace.",
   :no-doc true,
   :author "Michiel Borkent"}],
 :namespace-usages
 [{:filename "/tmp/foo.clj",
   :row 6,
   :col 14,
   :from foo,
   :to clojure.set}],
 :var-definitions
 [{:filename "/tmp/foo.clj",
   :row 8,
   :col 1,
   :ns foo,
   :name f,
   :private true,
   :fixed-arities #{1}}
  {:added "1.2",
   :ns foo,
   :name g,
   :var-args-min-arity 1,
   :filename "/tmp/foo.clj",
   :macro true,
   :col 1,
   :deprecated "1.3",
   :doc "No longer used.",
   :row 11}],
 :var-usages
 [{:filename "/tmp/foo.clj",
   :row 9,
   :col 3,
   :from foo,
   :to clojure.core,
   :name inc,
   :arity 1}
  {:filename "/tmp/foo.clj",
   :row 8,
   :col 1,
   :from foo,
   :to clojure.core,
   :name defn-,
   :arity 3}
  {:filename "/tmp/foo.clj",
   :row 16,
   :col 5,
   :from foo,
   :to clojure.core,
   :name comment}
  {:filename "/tmp/foo.clj",
   :row 11,
   :col 1,
   :from foo,
   :to clojure.core,
   :name defmacro,
   :arity 5}]}
```

NOTE: breaking changes may occur as result of feedback in the next few weeks (2019-07-30).

## Examples

These are examples of what you can do with the analysis data that clj-kondo
provides as a result of linting your sources.

To run the examples on your system you will need the Clojure [CLI
tool](https://clojure.org/guides/getting_started) version 1.10.1.466 or higher
and then use this repo as a git dep:

``` clojure
{:deps {clj-kondo/tools {:git/url "https://github.com/borkdude/clj-kondo"
                         :sha "44d54415b584694ff0e2dbfcbe71fd304b3829dd"
                         :deps/root "analysis"}}}
```

Replace the `:sha` with the latest SHA of this repo.

### Unused vars

``` shellsession
$ clj -m clj-kondo.tools.unused-vars src
The following vars are unused:
clj-kondo.tools.namespace-graph/-main
clj-kondo.tools.unused-vars/-main
```

A [planck](https://planck-repl.org) port of this example is available in the
`script` directory. You can invoke it like this:

``` shellsession
script/unused_vars.cljs src
```

### Private vars

A variation on the above tool, which looks at private vars and reports unused
private vars or illegally accessed private vars.

Example code:

``` clojure
(ns foo)

(defn- foo [])
(defn- bar []) ;; unused

(ns bar (:require [foo :as f]))

(f/foo) ;; illegal call
```

``` shellsession
$ clj -m clj-kondo.tools.private-vars /tmp/private.clj
/tmp/private.clj:4:8 warning: foo/bar is private but never used
/tmp/private.clj:8:1 warning: foo/foo is private and cannot be accessed from namespace bar
```

A [planck](https://planck-repl.org) port of this example is available in the
`script` directory. You can invoke it like this:

``` shellsession
script/private_vars.cljs /tmp/private.clj
```

### Namespace graph

This example requires GraphViz. Install with e.g. `brew install graphviz`.

``` shellsession
$ clj -m clj-kondo.tools.namespace-graph src
```

<img src="assets/namespace-graph.png">

### Find var

``` shellsession
$ clj -m clj-kondo.tools.find-var clj-kondo.core/run! src ../src
clj-kondo.core/run! is defined at ../src/clj_kondo/core.clj:51:7
clj-kondo.core/run! is used at ../src/clj_kondo/core.clj:120:12
clj-kondo.core/run! is used at ../src/clj_kondo/main.clj:81:44
clj-kondo.core/run! is used at src/clj_kondo/tools/find_var_usages.clj:8:29
clj-kondo.core/run! is used at src/clj_kondo/tools/namespace_graph.clj:7:29
clj-kondo.core/run! is used at src/clj_kondo/tools/unused_vars.clj:9:31
```

### Popular vars

``` shellsession
$ clj -m clj-kondo.tools.popular-vars 10 ../src
clojure.core/let: 196
clojure.core/defn: 183
clojure.core/when: 115
clojure.core/=: 86
clojure.core/if: 86
clojure.core/recur: 79
clojure.core/assoc: 70
clojure.core/or: 68
clojure.core/->: 68
clojure.core/first: 62
```

### Missing docstrings

``` shellsession
$ clj -m clj-kondo.tools.missing-docstrings ../src
clj-kondo.impl.findings/reg-finding!: missing docstring
clj-kondo.impl.findings/reg-findings!: missing docstring
clj-kondo.impl.namespace/reg-var!: missing docstring
clj-kondo.impl.namespace/reg-var-usage!: missing docstring
clj-kondo.impl.namespace/reg-alias!: missing docstring
...
```

### Circular dependencies

Code:

```
(ns a (:require b c))

(ns b (:require a)) ;; circular dependency

(ns c (:require a)) ;; circular dependency
```

```
$ clj -m clj-kondo.tools.circular-dependencies /tmp/circular.clj
/tmp/circular.clj:3:17: circular dependendy from namespace b to a
/tmp/circular.clj:5:17: circular dependendy from namespace c to a
```
