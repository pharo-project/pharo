tests
testMethodArgument
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new 
											encoderClass:  OpalEncoderForV3PlusClosures; 
											source: 'testMethodArgument
															| temp block |
															temp := 0.
															block := [ [ temp ] ].
															temp := 1.
															block := block value.
															temp := 2.
															self assert: block value = 2';
											class: OCMockCompilationClass;
											compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '17 <8A 01> push: (Array new: 1)'.
	self assert: (bytecode at: (index := index + 1)) = '19 <69> popIntoTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '20 <75> pushConstant: 0'.
	self assert: (bytecode at: (index := index + 1)) = '21 <8E 00 01> popIntoTemp: 0 inVectorAt: 1'.
	self assert: (bytecode at: (index := index + 1)) = '24 <11> pushTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '25 <8F 10 00 0A> closureNumCopied: 1 numArgs: 0 bytes 29 to 38'.
	self assert: (bytecode at: (index := index + 1)) = '29 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '30 <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes 34 to 37'.
	self assert: (bytecode at: (index := index + 1)) = '34 <8C 00 00> pushTemp: 0 inVectorAt: 0'.
	self assert: (bytecode at: (index := index + 1)) = '37 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '38 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '39 <68> popIntoTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '40 <76> pushConstant: 1'.
	self assert: (bytecode at: (index := index + 1)) = '41 <8E 00 01> popIntoTemp: 0 inVectorAt: 1'.
	self assert: (bytecode at: (index := index + 1)) = '44 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '45 <C9> send: value'.
	self assert: (bytecode at: (index := index + 1)) = '46 <68> popIntoTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '47 <77> pushConstant: 2'.
	self assert: (bytecode at: (index := index + 1)) = '48 <8E 00 01> popIntoTemp: 0 inVectorAt: 1'.
	self assert: (bytecode at: (index := index + 1)) = '51 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '52 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '53 <C9> send: value'.
	self assert: (bytecode at: (index := index + 1)) = '54 <77> pushConstant: 2'.
	self assert: (bytecode at: (index := index + 1)) = '55 <B6> send: ='.
	self assert: (bytecode at: (index := index + 1)) = '56 <E0> send: assert:'.
	self assert: (bytecode at: (index := index + 1)) = '57 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '58 <78> returnSelf'.


