# lingling-lang

* A series bare-minimun languages that adds incremental features. 
The first objective is String operation, since LingLing is famous for string instruments. 
It will not have built-in integers.
It will be music related after a long time.
To reach it's final form, requires practicing coding 40 hours a day.

## Steps
(unfinished)
* String literals `"lit"`
* Global names, expression, eval `(.(<- var "lit"))`
* Operations, expressions and eval
** Operations&Expression `(to_upper "lit")`
** Eval `(>> (to_upper "lit"))`
** Eg. `(<- var (to_upper "lit"))`
** Eg. `(<- var (>> (to_upper "lit")))`
* Do (>>> (<- var (to_upper "lit")) (<- var "litt"))

## Non-lisp version
* String literals `"lit"`
* Global names, expression
** `var <- "lit".`
** `var <- "lit"`
* Do operation `>> (var <- "lit") (klt <- var)`
* Operations, expressions and eval
** Operations&Expression `to upper: "lit"`
** Eval `(>> to upper: "lit")`
** Eg. `var <- (to upper: "lit")` or `var <- to upper: "lit"`
** Eg. `var <- (>> to upper: "lit")`
* Local names `env [var <- (to upper: "lit"), kar <- "grad"]`
* Macros
* 
