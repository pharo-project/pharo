tests
testSetUp
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
									encoderClass:  EncoderForV3PlusClosures; 
									source: 'setUp
												super setUp.
												collection := OrderedCollection new';
									class: OCMockCompilationClass;
									compile.
	
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) =  '21 <70> self'.
	self assert: (bytecode at: (index := index + 1)) =  '22 <85 00> superSend: setUp'.
	self assert: (bytecode at: (index := index + 1)) =  '24 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) =  '25 <41> pushLit: OrderedCollection'.
	self assert: (bytecode at: (index := index + 1)) =  '26 <CC> send: new'.
	self assert: (bytecode at: (index := index + 1)) =  '27 <60> popIntoRcvr: 0'.
	self assert: (bytecode at: (index := index + 1)) =  '28 <78> returnSelf'.
