tests
testPragma
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new 
									source: 'methodDoublePragma
											<hello: 5>
											<hello: 2>';
									class:  OCMockCompilationClass;
									compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = 'Quick return self'.

