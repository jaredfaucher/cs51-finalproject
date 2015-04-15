PROG = shamir

LIBS = \
	nums.cma \
	Core.Std

CAMLC = ocamlc
CAMLFLAGS = -g

%.cmo: %.ml
	$(CAMLC) $(CAMLFLAGS) -c $<

SOURCES = \
	Shamirint.ml \
	main.ml \

OBJECTS = $(SOURCES:.ml=.cmo)

$(PROG): $(OBJECTS)
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) -o $(PROG)

all: $(PROG)

clean:
	rm -rf *.cmo *.cmi $(PROG)
