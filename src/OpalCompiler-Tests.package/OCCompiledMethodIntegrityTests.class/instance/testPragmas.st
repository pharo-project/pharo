test
testPragmas

	| newCompiledMethod |
	
	newCompiledMethod := OpalCompiler new 
									source: 'methodDoublePragma
											<hello: 5>
											<hello: 2>';
									class: OCMockCompilationClass;
									compile.
									
	self assert: newCompiledMethod numArgs = 0.
	self assert: newCompiledMethod numLiterals = 2.
	
	"AdditionalState assertions"
	self assert: newCompiledMethod literals first selector = #methodDoublePragma.
	self assert: (newCompiledMethod literals first instVarNamed: 'method') = newCompiledMethod.
		
	self assert: newCompiledMethod primitive = 256.