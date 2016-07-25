tests
testToDoOutsideTempNotInlined
	"there seems to be a better indexzation of the temps we have one more"
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
										encoderClass:  OpalEncoderForV3PlusClosures; 
										source: 'testToDoOutsideTempNotInlined
														| block temp |
														block := [ :index | 
																	temp := index. 
																	collection add: [ temp ] ].
														1 to: 5 do: block.
														self assertValues: #(5 5 5 5 5)';
	class: OCMockCompilationClass;
	compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '33 <8A 01> push: (Array new: 1)'.
	self assert: (bytecode at: (index := index + 1)) = '35 <69> popIntoTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '36 <11> pushTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '37 <8F 11 00 10> closureNumCopied: 1 numArgs: 1 bytes 41 to 56'.
	self assert: (bytecode at: (index := index + 1)) = '41 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '42 <8E 00 01> popIntoTemp: 0 inVectorAt: 1'.
	self assert: (bytecode at: (index := index + 1)) = '45 <00> pushRcvr: 0'.
	self assert: (bytecode at: (index := index + 1)) = '46 <11> pushTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '47 <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes 51 to 54'.
	self assert: (bytecode at: (index := index + 1)) = '51 <8C 00 00> pushTemp: 0 inVectorAt: 0'.
	self assert: (bytecode at: (index := index + 1)) = '54 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '55 <E0> send: add:'.
	self assert: (bytecode at: (index := index + 1)) = '56 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '57 <68> popIntoTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '58 <76> pushConstant: 1'.
	self assert: (bytecode at: (index := index + 1)) = '59 <21> pushConstant: 5'.
	self assert: (bytecode at: (index := index + 1)) = '60 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '61 <F2> send: to:do:'.
	self assert: (bytecode at: (index := index + 1)) = '62 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '63 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '64 <23> pushConstant: #(5 5 5 5 5)'.
	self assert: (bytecode at: (index := index + 1)) = '65 <E4> send: assertValues:'.
	self assert: (bytecode at: (index := index + 1)) = '66 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '67 <78> returnSelf'.

	

