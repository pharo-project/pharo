*OpalCompiler-Core
classSideCompiler
	"Redefine this method if you want to customize the compiler for class-side methods"
	^self classSideCompilerClass new
		environment: self classSide environment;
		class: self classSide