tests
testReturnBlockInMethod
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
											encoderClass:  OpalEncoderForV3PlusClosures; 
											source: 'methodArgument: anObject
														^ [ anObject ]';
											class: OCMockCompilationClass;
											compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '13 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '14 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 18 to 19'.
	self assert: (bytecode at: (index := index + 1)) = '18 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '19 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '20 <7C> returnTop'.


