other-tests
testDifferentBlocksWithSameArgumentName
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
									encoderClass:  OpalEncoderForV3PlusClosures; 
									source: 'exampleNestedBlock
												| b c z |
												b := [:a | 
														z := 2.
														z + a].
												c := [:a |
														z + a].
											^ (b value: 2) + (c value: 1)';
									class: OCMockCompilationClass;
									compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '13 <8A 01> push: (Array new: 1)'.
	self assert: (bytecode at: (index := index + 1)) = '15 <6A> popIntoTemp: 2'.
	self assert: (bytecode at: (index := index + 1)) = '16 <12> pushTemp: 2'.
	self assert: (bytecode at: (index := index + 1)) = '17 <8F 11 00 0A> closureNumCopied: 1 numArgs: 1 bytes 21 to 30'.
	self assert: (bytecode at: (index := index + 1)) = '21 <77> pushConstant: 2'.
	self assert: (bytecode at: (index := index + 1)) = '22 <8E 00 01> popIntoTemp: 0 inVectorAt: 1'.
	self assert: (bytecode at: (index := index + 1)) = '25 <8C 00 01> pushTemp: 0 inVectorAt: 1'.
	self assert: (bytecode at: (index := index + 1)) = '28 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '29 <B0> send: +'.
	self assert: (bytecode at: (index := index + 1)) = '30 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '31 <68> popIntoTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '32 <12> pushTemp: 2'.
	self assert: (bytecode at: (index := index + 1)) = '33 <8F 11 00 06> closureNumCopied: 1 numArgs: 1 bytes 37 to 42'.
	self assert: (bytecode at: (index := index + 1)) = '37 <8C 00 01> pushTemp: 0 inVectorAt: 1'.
	self assert: (bytecode at: (index := index + 1)) = '40 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '41 <B0> send: +'.
	self assert: (bytecode at: (index := index + 1)) = '42 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '43 <69> popIntoTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '44 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '45 <77> pushConstant: 2'.
	self assert: (bytecode at: (index := index + 1)) = '46 <CA> send: value:'.
	self assert: (bytecode at: (index := index + 1)) = '47 <11> pushTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '48 <76> pushConstant: 1'.
	self assert: (bytecode at: (index := index + 1)) = '49 <CA> send: value:'.
	self assert: (bytecode at: (index := index + 1)) = '50 <B0> send: +'.
	self assert: (bytecode at: (index := index + 1)) = '51 <7C> returnTop'.