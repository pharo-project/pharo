public
decompile: aCompiledMethod
	"Answers a decompiled AST for the compiled method"
	^ self new decompileMethod: aCompiledMethod