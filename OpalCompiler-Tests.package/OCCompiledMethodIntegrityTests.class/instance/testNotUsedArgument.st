test
testNotUsedArgument

	| newCompiledMethod |
	
	newCompiledMethod := OpalCompiler new 
								source: 'ascentOf: aCharacter
											^ self ascent.';
								class: OCMockCompilationClass;
								compile.

	self assert: newCompiledMethod numArgs = 1.
	self assert: newCompiledMethod numLiterals = 3.
	self assert: newCompiledMethod numTemps = 1.
	self assert: newCompiledMethod primitive = 0.