### Instructions to build and install Capstone with its OCaml bindings

In this repository.

```bash
DIR=`pwd`
```

Install [_Capstone_](https://github.com/aquynh/capstone.git) from
source (in order to benefit from the OCaml bindings). For instance, on
an unix-based system:

```bash
https://github.com/aquynh/capstone.git
cd capstone
./make.sh
sudo ./make.sh install
# remove it with sudo ./make.sh uninstall
cd $PWD
```

Create an OCaml package findable by _findlib_.

```bash
cp META capstone/bindings/ocaml
cd capstone/bindings/ocaml
make
ocamlfind install capstone META capstone.* ocaml.*
# remove it with ocamlfind remove capstone
```
