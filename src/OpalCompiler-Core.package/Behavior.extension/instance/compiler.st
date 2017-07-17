*OpalCompiler-Core
compiler
	"Answer a compiler appropriate for source methods of this class."

	^self compilerClass new
		environment: self environment;
		class: self