*OpalCompiler-Core
generate
	"The receiver is the root of a parse tree. Answer a CompiledMethod. The
	argument, trailer, is the references to the source code that is stored with 
	every CompiledMethod."

	^ self generate: CompiledMethodTrailer empty