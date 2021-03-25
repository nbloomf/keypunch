# keypunch

In the olden days of computing, data was stored in patterns of holes on stiff pieces of paper called _punched cards_. Putting just the right holes in just the right places on these cards was fiddly and so the task was mediated by a special machine called a _keypunch_.

Though widely considered at best a lateral move, today in most applications the punched card has been replaced by the _PDF document_. And much like its predecessor the raw _PDF document format_ requires a fastidious hand to put every bit in its proper place, lest even the simplest documents be rendered (or rather not rendered) unreadable.

_keypunch_ is a low-level Haskell library for constructing well-formed PDF documents. Here _low-level_ means we are mainly concerned with exposing the smallest possible _semantic_ building blocks of PDFs while hiding the grittiest details involved in producing a sequence of bytes usable by PDF readers. Text layout algorithms, for instance, are beyond the scope, although it should be possible to implement one (or more!) using this library as a foundation. A key design goal is to produce PDFs which can be efficiently streamed, such as by a web server.

Current status: not useful for any purpose.

TODO:
- Better serialization for e.g. draw commands. cache length computation?
- Type safe names; e.g. font name
  - when a font name is expected, make it unpossible to use a different kind of name
  - addFont returns a tagged name, and setFont consumes it
- API for name and number trees
  - insert things into the name tree
  - convert the name tree to Objs
  - render the name tree with the catalog
