default:
	stack exec static-generator

build:
	stack build

%:
	stack exec static-generator $@
