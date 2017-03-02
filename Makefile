OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLYACCOPTIONS=-v
INCLUDES=                 # all relevant -I options here
OCAMLOPTIONS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTOPTIONS=$(INCLUDES) # add other options for ocamlopt here

PROGNAME=pbci

# The list of object files
COMMONOBJS=syntax.cmx gtfparser.cmx gtflexer.cmx typing.cmx eval.cmx
OBJS=$(COMMONOBJS) main.cmx
METAOBJS=$(COMMONOBJS) cogen.cmx metamain.cmx

DEPEND += gtflexer.ml gtfparser.ml

all: $(DEPEND) $(OBJS)
	$(OCAMLOPT) -o $(PROGNAME) $(OCAMLOPTIONS) $(OBJS)

meta:
	$(MAKE) OCAMLOPT=metaocamlopt OCAMLC=metaocamlc OBJS="$(METAOBJS)" all

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLOPTIONS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLOPTIONS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTOPTIONS) -c $<

gtfparser.ml gtfparser.mli: gtfparser.mly	
	@rm -f $@
	$(OCAMLYACC) $(OCAMLYACCOPTIONS) $<
	@chmod -w $@

gtflexer.ml: gtflexer.mll
	@rm -f $@
	$(OCAMLLEX) -o gtflexer.ml $<
	@chmod -w $@

# Clean up
clean:
	rm -f $(PROGNAME)
	rm -f *.cm[iox] *.o *~ parserx.ml parserx.mli parser.output lexerx.ml .depend

# Dependencies
depend:: $(DEPEND)
	$(OCAMLDEP) $(INCLUDES) -native *.mli *.ml > .depend

-include .depend
