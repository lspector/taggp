# taggp

Clojure code for tree-based genetic programming with tags
(see http://hampshire.edu/lspector/tags-gecco-2011/)

Lee Spector (lspector@hampshire.edu) 20120106-20120117

REQUIRES Clojure 1.3 for the concurrency to work (set single-thread-mode to true otherwise)

## Usage

Load an example and do (run) -- or invoke from leiningen to run the -main defined in 
an example...

e.g. lein run -m taggp.examples.parity :use-single-thread true

Arguments may be globals or other parameters set in the example file.



## Experimental features

Experimental features are placed in the src/taggp/exp directory. All modifications should be made in the appropriate name spaces. Namespaces that use dependencies that are being modified will need to be reloaded. To use experimental features in existing examples, add (use 'taggp.exp.[namspace]) to the main and reload dependencies using (use 'taggp.[dependency] :reload).

See [recursion.clj](https://github.com/lspector/taggp/blob/refactor/src/taggp/exp/recursion.clj) for an example of how to write experimental features.

See the main of <a href="https://github.com/lspector/taggp/blob/refactor/src/taggp/examples/parity.clj">parity.clj</a> for how to test experimental features in example problems.



## TODO

Move setting of globals from the example files into core.clj.



## License

Distributed under the Eclipse Public License, the same as Clojure.
