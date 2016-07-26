test
testPrimitive

	| newCompiledMethod |
	
	newCompiledMethod := OpalCompiler new 
								source: 'size
											<primitive: 62>
											^ self basicSize.';
								class: OCMockCompilationClass;
								compile.
							
	self assert: newCompiledMethod numArgs = 0.
	self assert: newCompiledMethod numLiterals = 3.
	self assert: newCompiledMethod numTemps = 0.
	self assert: newCompiledMethod primitive = 62.