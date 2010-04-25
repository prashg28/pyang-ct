PYANG = pyang -Wnone -p ../../modules --enable-complex-types

MODULES = $(wildcard *.yang)

test: clean
	@for m in $(MODULES); do 					\
		echo -n "checking $$m...";				\
		$(PYANG) $$m || exit 1;					\
		echo -n " generating xsd...";				\
		$(PYANG) -f xsd-ct -o $$m.xsd $$m || exit 1;		\
		echo " generating yin...";				\
		$(PYANG) -f yin -o $$m.yin $$m || exit 1;		\
		echo -n " ";						\
		echo -n " generating xsd from the generated yin...";	\
		$(PYANG) -f xsd-ct -o $$m.yin.xsd $$m.yin || exit 1;	\
		echo " comparing the two xsd...";			\
		diff $$m.xsd $$m.yin.xsd > $$m.diff || 			\
			{ cat $$m.diff; exit 1; };			\
		rm -f $$m.diff;						\
		echo -n " ";						\
		echo " ok";						\
	done 

clean:
	rm -rf *yin.xsd *yang.xsd *.yin *.gen.* *.cts
