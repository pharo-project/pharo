compiler
compiler
	^self compilerClass new
		compilationContextClass: CompilationContext;
		environment: self environment;
		class: self