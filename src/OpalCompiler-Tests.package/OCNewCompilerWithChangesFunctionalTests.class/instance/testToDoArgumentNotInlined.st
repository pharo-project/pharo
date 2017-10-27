tests
testToDoArgumentNotInlined
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
									encoderClass:  EncoderForV3PlusClosures; 
									source: 'testToDoArgumentNotInlined
													| block |
													block := [ :index |
														collection add: [ index ] ].
													1 to: 5 do: block.
													self assertValues: #(1 2 3 4 5)';
									class: OCMockCompilationClass;
									compile.
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '33 <8F 01 00 0A> closureNumCopied: 0 numArgs: 1 bytes 37 to 46'.
	self assert: (bytecode at: (index := index + 1)) = '37 <00> pushRcvr: 0'.
	self assert: (bytecode at: (index := index + 1)) = '38 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '39 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 43 to 44'.
	self assert: (bytecode at: (index := index + 1)) = '43 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '44 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '45 <E0> send: add:'.
	self assert: (bytecode at: (index := index + 1)) = '46 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '47 <68> popIntoTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '48 <76> pushConstant: 1'.
	self assert: (bytecode at: (index := index + 1)) = '49 <21> pushConstant: 5'.
	self assert: (bytecode at: (index := index + 1)) = '50 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '51 <F2> send: to:do:'.
	self assert: (bytecode at: (index := index + 1)) = '52 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '53 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '54 <23> pushConstant: #(1 2 3 4 5)'.
	self assert: (bytecode at: (index := index + 1)) = '55 <E4> send: assertValues:'.
	self assert: (bytecode at: (index := index + 1)) = '56 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '57 <78> returnSelf'.
