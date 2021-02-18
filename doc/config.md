# Configuration

For an overview of all available linters, go [here](linters.md).

Table of contents:

- [Introduction](#introduction)
- [Examples](#examples)
- [Unrecognized macros](#unrecognized-macros)
- [Hooks](#hooks)
- [Output](#output)
- [Example configurations](#example-configurations)
- [Exporting and importing configuration](#exporting-and-importing-configuration)
- [Deprecations](#deprecations)

## Introduction

Clj-kondo can be configured in five ways, by providing:

- home dir config in `~/.config/clj-kondo/config.edn` (respects `XDG_CONFIG_HOME`)
- project config: a `config.edn` file in the `.clj-kondo` directory (see
  [project setup](../README.md#project-setup))
- `:config-paths` in project `config.edn`: a list of directories that provide additional config
- command line `--config` file or EDN arguments
- namespace local config using `:clj-kondo/config` metadata in the namespace form

The configurations are merged in the following order, where a later config overrides an earlier config:

- home dir config
- `:config-paths` in project config
- project config
- command line config
- namespace local config

The `^:replace` metadata hint can be used to replace parts or all of the
configuration instead of merging with previous ones. The home dir config is
implicitly part of `:config-paths`. To opt out of merging with home dir config
use `:config-paths ^:replace []` in your project config.

Note that namespace local config must always be quoted: `{:clj-kondo/config
'{:linters ...}}` and quotes should not appear inside the config.

Look at the [default configuration](../src/clj_kondo/impl/config.clj) for all
available options.

## Examples

See the [config](https://github.com/clj-kondo/config) project for library-specific configurations.

## Unrecognized macros

Clj-kondo only expands a selected set of macros from clojure.core and some
popular community libraries. For unrecognized macros you can use these
configurations:

- [`:lint-as`](#lint-a-custom-macro-like-a-built-in-macro)
- [`:unresolved-symbol`](#exclude-unresolved-symbols-from-being-reported)
- [`:hooks`](#hooks)

## Linters

Each linter is identified by a keyword in the `:linters` config. We will start
with general linter configutation and will then enumerate all available linters
and corresponding config options.

### Enabling and disabling

#### Disable a linter

``` shellsession
$ echo '(select-keys [:a])' | clj-kondo --lint -
<stdin>:1:1: error: wrong number of args (1) passed to clojure.core/select-keys
linting took 10ms, errors: 1, warnings: 0

$ echo '(select-keys [:a])' | clj-kondo --lint - --config '{:linters {:invalid-arity {:level :off}}}'
linting took 10ms, errors: 0, warnings: 0
```

#### Enable optional linters

Some linters are not enabled by default. Right now these linters are:

- `:missing-docstring`: warn when public var doesn't have a docstring.
- `:unsorted-required-namespaces`: warn when namespaces in `:require` are not sorted.
- `:refer`: warn when there is **any** usage of `:refer` in your namespace requires.
- `:single-key-in`: warn when using assoc-in, update-in or get-in with single key.
- `:redundant-negation`: warn on redundant use of `not` or `complement`.
- `:shadowed-var`: warn when a binding shadows a var.

You can enable these linters by setting the `:level`:

``` clojure
{:linters {:missing-docstring {:level :warning}}}
```

#### Disable all linters but one

You can accomplish this by using `^:replace` metadata, which will override
instead of merge with other configurations:

``` shellsession
$ clj-kondo --lint corpus --config '^:replace {:linters {:redundant-let {:level :info}}}'
corpus/redundant_let.clj:4:3: info: redundant let
corpus/redundant_let.clj:8:3: info: redundant let
corpus/redundant_let.clj:12:3: info: redundant let
```

### Ignore warnings in an expression

To ignore all warnings in an expression, place a hint before it. It uses reader
comments, so they won't end up in your runtime.

``` clojure
#_:clj-kondo/ignore
(inc 1 2 3)
```

To ignore warnings from just one or a few linters:

``` clojure
#_{:clj-kondo/ignore [:invalid-arity]}
(inc 1 2 3)
```

To ignore warnings for only one language in a reader conditional:

``` clojure
#_{:clj-kondo/ignore #?(:clj [:unused-binding] :cljs [])}
(defn foo [x]
  #?(:cljs x)) ;; x is only used in cljs, but unused is ignored for clj, so no warning
```

### Lint a custom macro like a built-in macro

In the following code the `my-defn` macro is defined, but clj-kondo doesn't know how to interpret it:

``` clojure
(ns foo)

(defmacro my-defn [name args & body]
  `(defn ~name ~args
     (do (println "hello!")
       ~@body)))

(my-defn foo [x])
```

Hence `(foo 1 2 3)` will not lead to an invalid arity error. However, the syntax
of `my-defn` is a subset of `clojure.core/defn`, so for detecting arity errors
we might have just linted it like that. That is what the following configuration accomplishes:

``` clojure
{:lint-as {foo/my-defn clojure.core/defn}}
```

When you have custom `def` or `defn`-like macros and you can't find a supported macro that is like it, you can use:

``` clojure
{:lint-as {foo/my-defn clj-kondo.lint-as/def-catch-all}}
```

### Ignore the contents of comment forms

If you prefer not to lint the contents of `(comment ...)` forms, use this configuration:

```clojure
{:skip-comments true}
```

### Available linters

See [linters.md](linters.md).

## Hooks

See [hooks.md](hooks.md).

## Output

### Print results in JSON format


``` shell
$ clj-kondo --lint corpus --config '{:output {:format :json}}' | jq '.findings[0]'
{
  "type": "invalid-arity",
  "filename": "corpus/nested_namespaced_maps.clj",
  "row": 9,
  "col": 1,
  "level": "error",
  "message": "wrong number of args (2) passed to nested-namespaced-maps/test-fn"
}
```

Printing in EDN format is also supported.

### Print results with a custom format

``` shell
$ clj-kondo --lint corpus --config '{:output {:pattern "::{{level}} file={{filename}},line={{row}},col={{col}}::{{message}}"}}'
::warning file=corpus/compojure/core.clj,line=2,col=19::Unsorted namespace: foo
```

The custom pattern supports these template values:

| Template Variable | Notes                                                     |
|-------------------|-----------------------------------------------------------|
| `{{filename}}`    | File name                                                 |
| `{{row}}`         | Row where linter violation starts                         |
| `{{col}}`         | Column where linter violation starts                      |
| `{{level}}`       | Lowercase level of linter warning, one of info,warn,error |
| `{{LEVEL}}`       | Uppercase variant of `{{level}}`                          |
| `{{message}}`     | Linter message                                            |

### Include and exclude files from the output

``` shellsession
$ clj-kondo --lint "$(clj -Spath)" --config '{:output {:include-files ["^clojure/test"]}}'
clojure/test.clj:496:6: warning: redundant let
clojure/test/tap.clj:86:5: warning: redundant do
linting took 3289ms, errors: 0, warnings: 2

$ clj-kondo --lint "$(clj -Spath)" --config '{:output {:include-files ["^clojure/test"] :exclude-files ["tap"]}}'
clojure/test.clj:496:6: warning: redundant let
linting took 3226ms, errors: 0, warnings: 1
```

### Show progress bar while linting

``` shellsession
$ clj-kondo --lint "$(clj -Spath)" --config '{:output {:progress true}}'
.................................................................................................................
cljs/tools/reader.cljs:527:9: warning: redundant do
(rest of the output omitted)
```

### Output canonical file paths

The config `'{:output {:canonical-paths true}}'` will output canonical file
paths (absolute file paths without `..`). This also shows the full path of a jar
file when you lint a classpath.

``` shellsession
$ clj-kondo --lint corpus --config '{:output {:canonical-paths true}}'
/Users/borkdude/dev/clj-kondo/corpus/cljc/datascript.cljc:8:1: error: datascript.db/seqable? is called with 2 args but expects 1
(rest of the output omitted)
```

## Example configurations

These are some example configurations used in real projects. Feel free to create a PR with yours too.

- [clj-kondo](https://github.com/clj-kondo/clj-kondo/blob/master/.clj-kondo/config.edn)
- [rewrite-cljc](https://github.com/lread/rewrite-cljs-playground/blob/master/.clj-kondo/config.edn)

Also see the [config](https://github.com/clj-kondo/config) project.

## Exporting and importing configuration

Libraries can export configuration on the classpath. When a users lints using a
project classpath, these configurations are automatically detected and imported
into the `.clj-kondo` directory. To export config, make sure there is a
directory in your library with the following structure:

``` shellsession
clj-kondo.exports/<your-org>/<your-libname>
```

The [clj-kondo/config](https://github.com/clj-kondo/config) repo has
configurations and hook code for several libraries:

``` shellsession
$ tree -d -L 3 resources
resources
└── clj-kondo.exports
    └── clj_kondo
        ├── claypoole
        ├── fulcro
        ├── mockery
        ├── rum
        └── slingshot
```

Note that this library uses the org `clj-kondo` to not conflict with
configurations that the orgs of the libraries themselves might use for exporting
configuration. If the `claypoole` library itself wanted to export config, the
structure might look like:

``` shellsession
resources
└── clj-kondo.exports
    └── com.climate
        └── claypoole
```

Suppose you would have [clj-kondo/config](https://github.com/clj-kondo/config)
on your classpath and linted like this:

``` shellsession
$ clj-kondo --no-warnings --lint "$(clojure -Spath -Sdeps '{:deps {clj-kondo/config {:git/url "https://github.com/clj-kondo/config" :sha "e2e156c53c6c228fee7242629b41013f3e55051d"}}}')"
Copied config to .clj-kondo/clj-kondo/claypoole. Consider adding clj-kondo/claypoole to :config-paths in .clj-kondo/config.edn.
...
```

When configurations are found, instructions are printed how to opt in to those,
namely by adding the imported configs to `:config-paths` in
`.clj-kondo/config.edn`, like so:

``` shellsession
{:config-paths ["clj-kondo/claypoole"]}
```

Imported configurations can be checked into source control, at your convenience.

Note: configuration is only copied when all of these requirements are met:

- There is a `.clj-kondo` directory that can be used as a target.
- The `--no-warnings` flag is used to indicate that clj-kondo is used to populate the cache.

## Deprecations

Some configuration keys have been renamed over time. The default configuration
is always up-to-date and we strive to mantain backwards compatibility. However,
for completeness, you can find a list of the renamed keys here.

- `:if -> :missing-else-branch`
