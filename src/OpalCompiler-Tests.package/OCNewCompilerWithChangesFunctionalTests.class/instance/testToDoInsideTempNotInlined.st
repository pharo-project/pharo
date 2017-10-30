tests
testToDoInsideTempNotInlined
	"Some instructions are the same but we have a different number at he begining, and the storeTemp and popIntoTemp issue"
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
										encoderClass:  EncoderForV3PlusClosures; 
										source: 'testToDoInsideTempNotInlined
														| block |
														block := [ :index | 
																		| temp | 
																		temp := index. 
																		collection add: [ temp ] ].
														1 to: 5 do: block.
														self assertValues: #(1 2 3 4 5)';
	class: OCMockCompilationClass;
	compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '33 <8F 01 00 0D> closureNumCopied: 0 numArgs: 1 bytes 37 to 49'.
	self assert: (bytecode at: (index := index + 1)) = '37 <73> pushConstant: nil'.
	self assert: (bytecode at: (index := index + 1)) = '38 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '39 <69> popIntoTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '40 <00> pushRcvr: 0'.
	self assert: (bytecode at: (index := index + 1)) = '41 <11> pushTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '42 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 46 to 47'.
	self assert: (bytecode at: (index := index + 1)) = '46 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '47 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '48 <E0> send: add:'.
	self assert: (bytecode at: (index := index + 1)) = '49 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '50 <68> popIntoTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '51 <76> pushConstant: 1'.
	self assert: (bytecode at: (index := index + 1)) = '52 <21> pushConstant: 5'.
	self assert: (bytecode at: (index := index + 1)) = '53 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '54 <F2> send: to:do:'.
	self assert: (bytecode at: (index := index + 1)) = '55 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '56 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '57 <23> pushConstant: #(1 2 3 4 5)'.
	self assert: (bytecode at: (index := index + 1)) = '58 <E4> send: assertValues:'.
	self assert: (bytecode at: (index := index + 1)) = '59 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '60 <78> returnSelf'.



