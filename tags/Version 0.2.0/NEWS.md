# productivity 0.2.0
Changes in 'productivity' version 0.2.0 (2017-05-11)

## NEW FEATURES

* The `fareprim()` function now returns also shadow prices of input and output variables (`Shadowp`).

* Added a `NEWS.md` file to track changes to the package.

* Added a `CITATION` file.

## BUG FIXES

* In `fareprim()` function, arguments `by.year` and `by.id` can now be independently specified.
In the previous version of the package (v 0.1.0), specifying only one of these two arguments was generating an error (`object 'id.vec' not found`).

* In `lowe()` function, arguments `by.year` and `by.id` can now be independently specified.
In the previous version of the package (v 0.1.0), specifying only one of these two arguments was generating an error (`object 'id.vec' not found`).

## DEPRECATED & DEFUNCT
None

## OTHER USER-VISIBLE CHANGES

* Updated `DESCRIPTION` file.

* Updated `frameprim()` documentation to reflect the new `Shadowp` feature in `fareprim()`.

* Updated `frameprim()` documentation to correct minor typos.

* Updated `lowe()` documentation to correct minor typos.

* Updated startup message.


# productivity 0.1.0
First public release (2016-11-14)