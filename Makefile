OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
INCLUDES=                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

PROGNAME=pbci

# The list of object files
COMMONOBJS=syntax.cmx parserx.cmx lexerx.cmx typing.cmx eval.cmx
OBJS=$(COMMONOBJS) main.cmx
METAOBJS=$(COMMONOBJS) cogen.cmx metamain.cmx

DEPEND += lexerx.ml parserx.ml

all: $(DEPEND) $(OBJS)
	$(OCAMLOPT) -o $(PROGNAME) $(OCAMLFLAGS) $(OBJS)

meta:
	$(MAKE) OCAMLOPT=metaocamlopt OCAMLC=metaocamlc OBJS="$(METAOBJS)" all

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

parserx.ml parserx.mli: parser.mly	
	@rm -f $@
	$(OCAMLYACC) -v -b parserx $< 
	@chmod -w $@

lexerx.ml: lexer.mll
	@rm -f $@
	$(OCAMLLEX) -o lexerx.ml $<
	@chmod -w $@

# Clean up
clean:
	rm -f $(PROGNAME)
	rm -f *.cm[iox] *.o *~ parserx.ml parserx.mli parser.output lexerx.ml .depend

# Dependencies
depend:: $(DEPEND)
	$(OCAMLDEP) $(INCLUDES) -native *.mli *.ml > .depend

-include .depend
