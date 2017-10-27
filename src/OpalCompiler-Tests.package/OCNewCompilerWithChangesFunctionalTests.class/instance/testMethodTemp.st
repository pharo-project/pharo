tests
testMethodTemp
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
									encoderClass:  EncoderForV3PlusClosures; 
									source: 'testMethodTemp
													| block1 block2 |
													block1 := self methodArgument: 1.
													block2 := self methodArgument: 2.
													self assert: block1 value = 1.
													self assert: block2 value = 2';
									class: OCMockCompilationClass;
									compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '21 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '22 <76> pushConstant: 1'.
	self assert: (bytecode at: (index := index + 1)) = '23 <E0> send: methodArgument:'.
	self assert: (bytecode at: (index := index + 1)) = '24 <68> popIntoTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '25 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '26 <77> pushConstant: 2'.
	self assert: (bytecode at: (index := index + 1)) = '27 <E0> send: methodArgument:'.
	self assert: (bytecode at: (index := index + 1)) = '28 <69> popIntoTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '29 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '30 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '31 <C9> send: value'.
	self assert: (bytecode at: (index := index + 1)) = '32 <76> pushConstant: 1'.
	self assert: (bytecode at: (index := index + 1)) = '33 <B6> send: ='.
	self assert: (bytecode at: (index := index + 1)) = '34 <E1> send: assert:'.
	self assert: (bytecode at: (index := index + 1)) = '35 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '36 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '37 <11> pushTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '38 <C9> send: value'.
	self assert: (bytecode at: (index := index + 1)) = '39 <77> pushConstant: 2'.
	self assert: (bytecode at: (index := index + 1)) = '40 <B6> send: ='.
	self assert: (bytecode at: (index := index + 1)) = '41 <E1> send: assert:'.
	self assert: (bytecode at: (index := index + 1)) = '42 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '43 <78> returnSelf'.

