I provide the API of the whole Compiler Package.

-> parsing: just parse
-> translate: parse and generate code so we get all error messages
-> compile: translate but return the CompiledMethod 

Example:

OpalCompiler new
	source: 'test 1+2';
	class: Object;
	compile.

This returns a CompiledMethod.