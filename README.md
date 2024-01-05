# thingatpt-utils core

[![CircleCI thingatpt-utils-core](https://circleci.com/gh/andreas-roehler/thingatpt-utils-core.svg?style=svg)](https://app.circleci.com/pipelines/gh/andreas-roehler/thingatpt-utils-core)

===

# Return, mover over or manipulate a THING. 

THING may be a well known
form as word, paragraph, but also a char class as
`alnum' or a new defined thing.

For example ‘ar-alnum-atpt’ will return all
alpha-numerical chars below and around cursor as a
string. ‘ar-bounds-of-alnum-atpt’ returns the
borders of that string as a list and so on.

‘ar-delimited-atpt’ will fetch buffer content, if point is between or
at delimiter chars. See customizable vars
‘th-beg-delimiter’, ‘th-end-delimiter’, ‘ar-delimiters-atpt’.

Presently for a given THING the following is
implemented:

ar-THING-atpt
ar-THING-bounds-atpt
ar-THING-beginning-position-atpt
ar-THING-end-position-atpt
ar-THING-backward-atpt
ar-THING-forward-atpt
ar-THING-length-atpt
ar-THING-copy-atpt
ar-THING-kill-atpt
ar-THING-backward-atpt

A THING defined by unary delimiters like doublequotes: 
cursor needs to be between or at the start.
