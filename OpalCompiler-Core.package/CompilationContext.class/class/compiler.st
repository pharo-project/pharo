compiler
compiler
	^self compilerClass new
		compilationContextClass: CCompilationContext;
		environment: self environment;
		class: self