## Linters

This page contains an overview of all available linters and their corresponding
configuration. For general configurations options, go [here](config.md).

Table of contents:
- [Linters](#linters)
  - [Cond-else](#cond-else)
  - [Conflicting-alias](#conflicting-alias)
  - [Consistent-alias](#consistent-alias)
  - [Datalog syntax](#datalog-syntax)
  - [Deprecated var](#deprecated-var)
  - [Deps.edn](#depsedn)
  - [Duplicate map key](#duplicate-map-key)
  - [Duplicate require](#duplicate-require)
  - [Duplicate set key](#duplicate-set-key)
  - [File](#file)
  - [Format](#format)
  - [Inline def](#inline-def)
  - [Invalid arity](#invalid-arity)
  - [Misplaced docstring](#misplaced-docstring)
  - [Missing body in when](#missing-body-in-when)
  - [Missing clause in try](#missing-clause-in-try)
  - [Missing docstring](#missing-docstring)
  - [Missing else branch](#missing-else-branch)
  - [Missing map value](#missing-map-value)
  - [Missing test assertion](#missing-test-assertion)
  - [Not empty?](#not-empty)
  - [Private call](#private-call)
  - [Redefined var](#redefined-var)
  - [Redundant do](#redundant-do)
  - [Redundant let](#redundant-let)
  - [Redundant negation](#redundant-negation)
  - [Refer](#refer)
  - [Refer all](#refer-all)
  - [Single key in](#single-key-in)
  - [Single operand comparison](#single-operand-comparison)
  - [Shadowed var](#shadowed-var)
  - [Syntax](#syntax)
  - [Type mismatch](#type-mismatch)
  - [Unbound destructuring default](#unbound-destructuring-default)
  - [Unused binding](#unused-binding)
  - [Unreachable code](#unreachable-code)
  - [Unused import](#unused-import)
  - [Unresolved namespace](#unresolved-namespace)
  - [Unresolved symbol](#unresolved-symbol)
  - [Unresolved var](#unresolved-var)
  - [Unsorted required namespace](#unsorted-required-namespace)
  - [Unused namespace](#unused-namespace)
  - [Unused private var](#unused-private-var)
  - [Unused referred var](#unused-referred-var)
  - [Use](#use)

### Cond-else

*Keyword:* `:cond-else`.

*Description:* warn on `cond` with a different constant for the else branch than `:else`.

*Default level:* `:warning`.

*Example trigger:* `(cond (odd? (rand-int 10)) :foo :default :bar)`.

*Example message:* `use :else as the catch-all test expression in cond`.

### Conflicting-alias

*Keyword:* `:conflicting-alias`.

*Description:* warn on conflicting alias.

*Default level:* `:error`.

*Example trigger:*

``` clojure
(require '[clojure.string :as s]
         '[clojure.spec.alpha :as s])
```

*Example message:* `Conflicting alias for clojure.spec.alpha`.

### Consistent-alias

*Keyword:* `:consistent-alias`

*Description:* Sometimes it's desirable to have a consistent alias for certain
namespaces in a project. E.g. in the below code it could be desirable if every
alias for `old.api` was `old-api`:

*Default level:* `:warning`.

*Example trigger:*

``` clojure
(ns foo (:require [new.api :as api]))
(ns bar (:require [old.api :as old-api]))
(ns baz (:require [old.api :as api]))
```

*Config:*

The consistent alias linter needs pre-configured aliases for namespaces that
should have a consistent alias. This configuration:

``` clojure
{:linters {:consistent-alias {:aliases {old.api old-api}}}}
```

will produce this warning:

``` clojure
Inconsistent alias. Expected old-api instead of api.
```

### Datalog syntax

*Keyword:* `:datalog-syntax`.

*Description:* warn on invalid datalog syntax. This linter is implemented using
[io.lambdaforge/datalog-parser](https://github.com/lambdaforge/datalog-parser). Also
see this [blog
post](https://lambdaforge.io/2019/11/08/clj-kondo-datalog-support.html).

*Default level:* `:error`.

*Example trigger:*

``` clojure
(ns user (:require [datahike.api :refer [q]]))

(q '[:find ?a :where [?b :foo _]] 42)
```

*Example message:* `Query for unknown vars: [?a]`.

### Deprecated var

*Keyword:* `:deprecated-var`.

*Description:* warn on usage of var that is deprecated.

*Default level:* `:warning`.

*Example trigger:* `(def ^:deprecated x) x`

Example warning: `#'user/x is deprecated`.

*Config:*

Say you have the following function:

``` clojure
(ns app.foo)
(defn foo {:deprecated "1.9.0"} [])
```

and you still want to be able to call it without getting a warning, for example
in function in the same namespace which is also deprecated:

``` clojure
(defn bar {:deprecated "1.9.0"} []
  (foo))
```

or in test code:

``` clojure
(ns app.foo-test
  (:require
   [app.foo :refer [foo]]
   [clojure.test :refer [deftest is]]))

(deftest foo-test [] (is (nil? (foo))))
```

To achieve this, use this config:

``` clojure
{:linters
 {:deprecated-var
  {:exclude
   {app.foo/foo
    {:defs [app.foo/bar]
     :namespaces [app.foo-test]}}}}}
```

A regex is also permitted, e.g. to exclude all test namespaces:

``` clojure
{:linters {:deprecated-var {:exclude {app.foo/foo {:namespaces [".*-test$"]}}}}}
```

### Deps.edn

*Keyword:* `:deps.edn`

*Description:* warn on common errors in a `deps.edn`.

*Default level:* `:warning`

*Example trigger:*

`deps.edn`:

``` clojure
{:deps {foo/bar "2020.10.11"}}
```

*Example message:*

```
Expected map, found: java.lang.String
```

### Duplicate map key

*Keyword:* `:duplicate-map-key`.

*Description:* warn on duplicate key in map.

*Default level:* `:error`.

*Example trigger:* `{:a 1 :a 2}`

*Example message:* `duplicate key :a`.

### Duplicate require

*Keyword:* `:duplicate-require`.

*Description:* warns on namespace that has been required more than once within a namespace.

*Example trigger:*

``` clojure
(ns foo
  (:require [clojure.string :as str]
            [clojure.string :as str]))
```

*Example message:* `duplicate require of clojure.string`

### Duplicate set key

*Keyword:* `:duplicate-set-key`.

*Description:* similar to `:duplicate-map-key` but for sets.

*Example trigger:* `#{:a :a}`

*Example message:* `duplicate set element :a`.

### File

*Keyword:* `:file`.

*Description:* warn on error while reading file.

*Default level:* `:error`.

*Example trigger:* `clj-kondo --lint foo.clje`.

*Example message:* `file does not exist`.

### Format

*Keyword:* `:format`.

*Description:* warn on unexpected amount of arguments in `format`.

*Default level:* `:error`.

*Example trigger:* `(format "%s" 1 2)`.

*Example message:* `Format string expects 1 arguments instead of 2.`.

### Inline def

*Keyword:* `:inline-def`.

*Description:* warn on non-toplevel usage of `def` (and `defn`, etc.).

*Default level:* `:warning`.

*Example trigger:* `(defn foo [] (def x 1))`.

*Example message:* `inline def`.

### Invalid arity

**Keyword:** `:invalid-arity`.

*Description:* warn when a function (or macro) is called with an invalid amount of
arguments.

*Default level:* `:error`.

*Example trigger:* `(inc)`.

*Example message:* `clojure.core/inc is called with 0 args but expects 1`.

*Config:*

Some macros rewrite their arguments and therefore can cause false positive arity
errors. Imagine the following silly macro:

``` clojure
(ns silly-macros)

(defmacro with-map [m [fn & args]]
  `(~fn ~m ~@args))
```

which you can call like:

``` clojure
(silly-macros/with-map {:a 1 :d 2} (select-keys [:a :b :c])) ;;=> {:a 1}
```

Normally a call to this macro will give an invalid arity error for `(select-keys
[:a :b :c])`, but not when you use the following configuration:

``` clojure
{:linters {:invalid-arity {:skip-args [silly-macros/with-map]}}}
```

### Misplaced docstring

*Keyword:* `:misplaced-docstring`.

*Description:* warn when docstring appears after argument vector instead of before.

*Default level:* `:warning`.

*Example trigger:* `(defn foo [] "cool fn" 1)`.

*Example message:* `Misplaced docstring.`

### Missing body in when

*Keyword:* `:missing-body-in-when`.

*Description:* warn when `when` is called only with a condition.

*Default level:* `:warning`.

*Example trigger:* `(when true)`.

*Example message:* `Missing body in when`.

### Missing clause in try

*Keyword:* `:missing-clause-in-try`.

*Description:* warn when `try` expression misses `catch` or `finally` clause.

*Default level:* `:warning`.

*Example trigger:* `(try 1)`.

*Example message:* `Missing catch or finally in try.`

### Missing docstring

*Keyword:* `:missing-docstring`.

*Description:* warn when public var misses docstring.

*Default level:* `:off`.

*Example trigger:* `(defn foo [] 1)`.

*Example message:* `Missing docstring.`

### Missing else branch

*Keyword:* `:missing-else-branch`.

*Description:* warns about missing else branch in `if` expression.

*Default level:* `:warning`.

*Example trigger:* `(if :foo :bar)`.

*Example message:* `Missing else branch..`

### Missing map value

*Keyword:* `:missing-map-value`.

*Description:* warn on key with uneven amount of elements, i.e. one of the keys
misses a value.

*Default level:* `:error`.

*Example trigger:* `{:a 1 :b}`

*Example message:* `missing value for key :b`.

### Missing test assertion

*Keyword:* `:missing-test-assertion`.

*Description:* warn on `deftest` expression without test assertion.

*Default level:* `:warning`.

*Example trigger:*

``` clojure
(require '[clojure.test :as test])
(test/deftest foo (pos? 1))
```

*Example message:* `missing test assertion`.

### Not empty?

*Keyword:* `:not-empty?`

*Description:* warn on `(not (empty? ...))` idiom. According to the docstring of `empty?` `seq` is prefered.

*Default level:* `:warning`.

*Example trigger:* `(not (empty? []))`

*Example message:* `use the idiom (seq x) rather than (not (empty? x))`.

### Private call

Keyword `:private-call`.

*Description:* warn when private var is used. The name of this linter should be
renamed to "private usage" since it will warn on usage of private vars and not
only inside calls.

*Default level:* `:error`.

*Example trigger:*

``` clojure
(ns foo) (defn- f [])

(ns bar (:require [foo]))
(foo/f)
```

*Example message:* `#'foo/f is private`.

To suppress the above message, refer to `foo/f` using the var `#'foo/f` or write:

``` shellsession
#_{:clj-kondo/ignore [:private-call]}
(foo/f)
```

### Redefined var

*Keyword:* `:redefined-var`.

*Description:* warn on redefind var.

*Default level:* `:warning`.

*Example trigger:* `(def x 1) (def x 2)`

*Example message:* `redefined var #'user/x`.

### Redundant do

*Keyword:* `:redundant-do`.

*Description:* warn on usage of do that is redundant. The warning usually arises
because of an explicit or implicit do as the direct parent s-expression.

*Default level:* `:warning`.

*Example trigger:* `(defn foo [] (do 1))`.

*Example message:* `redundant do`.

### Redundant let

*Keyword:* `:redundant-let`.

*Description:* warn on usage of let that is redundant. The warning usually arises
because directly nested lets.

*Default level:* `:warning`.

*Example trigger:* `(let [x 1] (let [y 2] (+ x y)))`.

*Example message:* `Redundant let expression.`

### Redundant negation

*Keyword:* `:redundant-negation`.

*Description:* warn on usage of `not` or `complement` that is redundant. This includes
`and` & `or` used with multiple `not`s, negated core fns where their complement is
already available, and `filter` and `remove` used with negated fns. *N.b.* `:not-empty`
is a separate (and default enabled) linter.

*Default level:* `:off`.

*Example trigger:* `(and (not (string? "foo")) (not (keyword? :bar)))`.

*Example message:* `And & 2 nots used instead of 1 not with or`.

*Example trigger:* `(not (nil? foo))`.

*Example message:* `not and nil? used instead of some?`.

*Example trigger:* `(filter #(not (string? %)) [:foo "bar" 42])`.

*Example message:* `filter and not used instead of remove`.

### Refer

*Keyword:* `:refer`

*Description:* warns when `:refer` is used. This can be used when one wants to
enforce usage of aliases.

*Default level:* `:off`.

*Example trigger:* `(ns foo (:require [clojure.set :refer [union]]))`.

Example warning: `require with :refer`.

### Refer all

*Keyword:* `:refer-all`

*Description:* warns when `:refer :all` is used.

*Default level:* `:warning`.

*Example trigger:* `(ns foo (:require [clojure.set :refer :all]))`.

*Example message:* `use alias or :refer`.

*Config:* to suppress the above warning:

```clojure
{:linters {:refer-all {:exclude [clojure.set]}}}
```

### Single key in

*Keyword:* `:single-key-in`.

*Description:* warn on associative path function with a single value path.

*Default level:* `:off`.

*Example trigger:* `(get-in {:a 1} [:a])`.

*Example message:* `get-in with single key.`

### Single operand comparison

*Keyword:* `:single-operand-comparison`.

*Description:* warn on comparison with only one argument.

*Default level:* `:warning`.

*Example trigger:* `(< 1)`.

*Example message:* `Single operand use of clojure.core/< is always true.`

### Shadowed var

*Keyword:* `:shadowed-var`.

*Description:* warn on var that is shadowed by local.

*Default level:* `:off`.

*Example trigger:* `(def x 1) (let [x 2] x)`.

*Example message:* `Shadowed var: user/x.`

*Config:*

``` clojure
{:linters {:shadowed-var {:level :warning
                          :exclude [ns]
                          :suggest {name nom}}}}
```

``` clojure
(fn [name] name)
     ^--- Shadowed var: clojure.core/name. Suggestion: nom
```

Use `:exclude` to suppress warnings for specific binding names. Use `:include`
to warn only for specific names.

To avoid shadowing core vars you can also use `:refer-clojure` + `:exclude` in
the `ns` form.

### Syntax

*Keyword:* `:syntax`.

*Description:* warn on invalid syntax.

*Default level:* `:warning`.

*Example trigger:* `[)`.

Example messages:

```
Mismatched bracket: found an opening [ and a closing ) on line 1
Mismatched bracket: found an opening [ on line 1 and a closing )
```

### Type mismatch

*Keyword:* `:type-mismatch`.

*Description:* warn on type mismatches, e.g. passing a keyword where a number is expected.

*Default level:* `:error`.

*Example trigger:* `(inc :foo)`

*Example message:* `Expected: number, received: keyword.`

*Config:*

You can add or override type annotations. See
[types.md](https://github.com/clj-kondo/clj-kondo/blob/master/doc/types.md).

### Unbound destructuring default

*Keyword:* `:unbound-destructuring-default`.

*Description:* warn on binding in `:or` which does not occur in destructuring.

*Default level:* `:warning`.

*Example trigger:* `(let [{:keys [:i] :or {i 2 j 3}} {}] i)`

*Example message:* `j is not bound in this destructuring form`.

### Unused binding

*Keyword:* `:unused-binding`.

*Description:* warn on unused binding.

*Default level:* `:warning`.

*Example trigger:* `(let [x 1] (prn :foo))`

*Example message:* `unused binding x`.

*Config:*

To exclude unused bindings from being reported, start their names with
underscores: `_x`.

To exclude warnings about key-destructured function arguments, use:

``` clojure
{:linters {:unused-binding {:exclude-destructured-keys-in-fn-args true}}}
```

This will disable warnings for the following example:

``` clojure
(defn f [{:keys [:a :b :c]} d])
```

To disable warnings about `:as` bindings (which can be useful for
documentation), use:

```clojure
{:linters {:unused-binding {:exclude-destructured-as true}}}
```

This will disable the warning in:

``` clojure
(defn f [{:keys [a b c] :as g}] a b c)
```

### Unreachable code

*Keyword:* `:unreachable-code`.

*Description:* warn on unreachable code.

*Default level:* `:warning`.

*Example trigger:* `(cond :else 1 (odd? 1) 2)`.

*Example message:* `unreachable code`.

### Unused import

*Keyword:* `:unused-import`.

*Description:* warn on unused import.

*Default level:* `:warning`.

*Example trigger:* `(ns foo (:import [java.util UUID]))`.

*Example message:* `Unused import UUID.`

### Unresolved namespace

*Keyword:* `:unresolved-namespace`.

*Default level:* `:error`.

*Example trigger:* `foo.bar/baz`.

*Example message:* `Unresolved namespace foo.bar. Are you missing a require?`

*Config:* use `:exclude [foo.bar]` to suppress the above warning.

### Unresolved symbol

*Keyword:* `:unresolved-symbol`.

*Default level:* `:error`.

*Example trigger:* `x`.

*Example message:* `Unresolved symbol: x`.

*Config:*

In the following code `streams` is a macro that assigns a special meaning to the
symbol `where`, so it should not be reported as an unresolved symbol:

``` clojure
(ns foo
  (:require [riemann.streams :refer [streams]]))

(def email (mailer {:host "mail.relay"
                    :from "riemann@example.com"}))
(streams
  (where (and (= (:service event) “my-service”)
              (= (:level event) “ERROR”))
    ,,,))
```

This is the config for it:

``` clojure
{:linters
  {:unresolved-symbol
    {:exclude [(riemann.streams/streams [where])]}}}
```

To exclude all symbols in calls to `riemann.streams/streams` write `:exclude [(riemann.streams/streams)]`, without the vector.

To exclude a symbol from being reported as unresolved globally in your project, e.g. `foo`, you can use `:exclude [foo]`.

Sometimes vars are introduced by executing macros, e.g. when using [HugSQL](https://github.com/layerware/hugsql)'s `def-db-fns`. You can suppress warnings about these vars by using `declare`. Example:

``` clojure
(ns hugsql-example
  (:require [hugsql.core :as hugsql]))

(declare select-things)

;; this will define a var #'select-things:
(hugsql/def-db-fns "select_things.sql")

(defn get-my-things [conn params]
  (select-things conn params))
```

If the amount of symbols introduced by HugSQL becomes too unwieldy, consider
introducing a separate namespace in which HugSQL generates the vars:
`foo.db.hugsql`. You can then refer to this namespace from `foo.db` with
`(require '[foo.db.hugsql :as sql]) (sql/insert! ...)` and clj-kondo will not
complain about this.

Furthermore, the `:lint-as` option can help treating certain macros like
built-in ones. This is in clj-kondo's own config:

``` clojure
:lint-as {me.raynes.conch/programs clojure.core/declare
          me.raynes.conch/let-programs clojure.core/let}
```

and helps preventing false positive unresolved symbols in this code:

``` clojure
(ns foo (:require [me.raynes.conch :refer [programs let-programs]]))

(programs rm mkdir echo mv)
(let-programs [clj-kondo "./clj-kondo"]
  ,,,)
```

### Unresolved var

*Keyword:* `:unresolved-var`.

*Description:* warns on unresolved var from other namespace.

*Default level:* `:warning`.

*Example trigger:* `(require '[clojure.set :as set]) (set/onion)`.

*Example message:* `Unresolved var: set/onion`.

*Config:*

Given this example:

``` clojure
(ns foo)
(defmacro gen-vars [& names]) (gen-vars x y z)

(ns bar (:require foo))
foo/x
(foo/y)
```

you can exclude warnings for all unresolved vars from namespace `foo` using:

``` clojure
{:linters {:unresolved-var {:exclude [foo]}}}
```

or exclude a selection of unresolved vars using qualified symbols:

``` clojure
{:linters {:unresolved-var {:exclude [foo/x]}}}
```

### Unsorted required namespace

*Keyword:* `:unsorted-required-namespace`.

*Description:* warns on non-alphabetically sorted libspecs in `ns` and `require` forms.

*Default level:* `:off`.

*Example trigger:* `(ns foo (:require b a))`.

*Example message:* `Unsorted namespace: a`.

### Unused namespace

*Keyword:* `:unused-namespace`.

*Description:* warns on required but unused namespace.

*Default level:* `:warning`.

*Example trigger:* `(ns foo (:require [bar :as b]))`.

*Example message:* `namespace bar is required but never used`.

*Config:*

Given this example:

``` clojure
(ns foo (:require [foo.specs :as specs]))
```

you will get a warning about `foo.specs` being unused.

To suppress this, you can either leave out the alias `specs` if it isn't used
anywhere in the namespace or use this config:

``` clojure
{:linters {:unused-namespace {:exclude [foo.specs]}}}
```

A regex is also supported:

``` clojure
{:linters {:unused-namespace {:exclude [".*\\.specs$"]}}}
```

This will exclude all namespaces ending with `.specs`.

### Unused private var

*Keyword:* `:unused-private-var`.

*Description:* warns on unused private vars.

*Default level:* `:warning`.

*Example trigger:* `(ns foo) (defn- f [])`

*Example message:* `Unused private var foo/f`

*Config:*

To suppress the above warning:

``` clojure
{:linters {:unused-private-var {:exclude [foo/f]}}}
```

### Unused referred var

*Keyword:* `:unused-referred-var`.

*Description:* warns about unused referred vars.

*Default level:* `:warning`.

*Example trigger:* `(ns foo (:require [clojure.set :refer [union]]))`.

*Example message:* `#'clojure.set/union is referred but never used`.

*Config:*

Imagine you want to have `taoensso.timbre/debug` available in all of your
namespaces. Even when you don't use it, you don't want to get a warning about
it. That can be done as follows:

``` clojure
{:linters {:unused-referred-var {:exclude {taoensso.timbre [debug]}}}}
```

### Use

*Keyword:* `:use`.

*Description:* warns about `:use` or `use`.

*Default level:* `:warning`.

*Example trigger:* `(ns foo (:use [clojure.set]))`.

*Example message:* `use :require with alias or :refer`.

