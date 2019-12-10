ocamlfind ocamlc -linkpkg -package delimcc -o main main.ml
ocamlfind -toolchain metaocaml ocamlc -linkpkg -package delimcc -o metac main_meta.ml
./metac > meta_out.ml
ocamlopt -O3 -o meta meta_out.ml
