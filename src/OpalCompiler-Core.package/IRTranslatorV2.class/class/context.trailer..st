instance creation
context: aCompilationContext trailer: aCompiledMethodTrailer
	^self basicNew 
		initialize;
		compilationContext: aCompilationContext;
		trailer: aCompiledMethodTrailer