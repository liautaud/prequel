![Prequel](http://amaia.at/prequel.png)

*A minimalistic SQL engine for CSV files.*


# About this project.

This project is an OCaml implementation of a SQL engine for CSV files.

It was written for the [DBDM course](https://perso.liris.cnrs.fr/emmanuel.coquery/dbdm/DBDM-2018-project.html) of the ENS de Lyon.


# Installation guide.

This project depends on a few packages, which can be installed using [OPAM](https://opam.ocaml.org/doc/Install.html):
```
opam install obuild
opam install ppx_deriving
opam install csv
```

Then, building the project is as simple as running:
```
obuild configure && obuild build
```


# Usage.

To play around with the engine, just run `./repl` to get an SQLite-like REPL.

For better usability, install `rlwrap` and run `rlwrap ./repl` instead.

Here are a few examples of requests:

- `SELECT * FROM "tests/projets.csv" projets;`
- `SELECT * FROM "tests/projets.csv" projets WHERE idp > responsable;`
- `SELECT idp AS foo FROM "tests/projets.csv" projets WHERE foo > responsable;`