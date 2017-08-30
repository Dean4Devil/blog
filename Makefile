all:
	stack exec static-generator
%:
	stack exec static-generator $@
