.PHONY: build default
all: build
	stack exec static-generator
build:
	@stack build

.DEFAULT:
	@$(MAKE) --no-print-directory build
	stack exec static-generator $@
