test
testBlockTemps

	| newCompiledMethod |
	newCompiledMethod := OpalCompiler new 
								source: 'ascentOf: aCharacter
											^ [ | temp1 temp2 temp3 | 1= temp1. 2 = temp2. 3 = temp3].';
								class: OCMockCompilationClass;
								compile.
								
	self assert: newCompiledMethod numArgs = 1.
	self assert: (newCompiledMethod numLiterals = 3 or: [newCompiledMethod numLiterals = 4 "Sista Bytecode" ]).
	self assert: newCompiledMethod numTemps = 1.
	self assert: newCompiledMethod primitive = 0.