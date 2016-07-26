public api
decompile: aCompiledMethod
	"Main API. Works only if the method holds a reference to the class it is installed in and its selector. Answers an AST"
	^ self
		decompile: aCompiledMethod selector
		in: aCompiledMethod methodClass
		method: aCompiledMethod