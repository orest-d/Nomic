Nomic
=====

Nomic - a toolset for Domain Specific Languages

Nomic processes a DSL description called meno (Meta Nomic). The DSL description (.meno file) is very similar to a grammar definition (e.g. Backus–Naur form), but adds small restrictions and requirements which allow to build from the single source a lot of code that would otherwise have to be created manually:

* Abstract Syntac Tree objects
* Parser, that automatically creates the AST tree (Rather than being a parser generator, Nomic generates an input for a parser generator. Therefore it can leverage on existing parsing infrastructure.)
* Translator (compiler), which translates AST back to the original source. This can be used as a starting point to build a translator to another form.
* Trivial interpreter, which interprets the AST objects.
* Unification objects, which allow to build complex type-safe patterns for matching AST objects. Pattern matching can be used to identify standardized constructions (e.g. a loop construction)  and convert them to another form. Another use for pattern matching is optimization.
* Prolog description (experimental)

Nomic can be used to quickly protorype and build programming languaged (transpilers, compilers and interpreters) and other formal languages (e.g. declarative user interface description, data binding, data description languages etc.).

Projects similar to Nomic:
--------------------------

* http://strategoxt.org/Spoofax/WebHome
* https://www.eclipse.org/Xtext/
