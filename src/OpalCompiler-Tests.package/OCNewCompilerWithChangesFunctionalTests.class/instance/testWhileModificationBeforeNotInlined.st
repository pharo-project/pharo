tests
testWhileModificationBeforeNotInlined
	"The bytecodes integers are different in some cases."
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
										encoderClass:  EncoderForV3PlusClosures; 
										source: 'testWhileModificationBeforeNotInlined
														| index block |
														index := 0.
														block := [ 
																index := index + 1.
																collection add: [ index ] ].
														[ index < 5 ] whileTrue: block.
														self assertValues: #(5 5 5 5 5)';
	class: OCMockCompilationClass;
	compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '33 <8A 01> push: (Array new: 1)'.
	self assert: (bytecode at: (index := index + 1)) = '35 <69> popIntoTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '36 <75> pushConstant: 0'.
	self assert: (bytecode at: (index := index + 1)) = '37 <8E 00 01> popIntoTemp: 0 inVectorAt: 1'.
	self assert: (bytecode at: (index := index + 1)) = '40 <11> pushTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '41 <8F 10 00 14> closureNumCopied: 1 numArgs: 0 bytes 45 to 64'.
	self assert: (bytecode at: (index := index + 1)) = '45 <8C 00 00> pushTemp: 0 inVectorAt: 0'.
	self assert: (bytecode at: (index := index + 1)) = '48 <76> pushConstant: 1'.
	self assert: (bytecode at: (index := index + 1)) = '49 <B0> send: +'.
	self assert: (bytecode at: (index := index + 1)) = '50 <8E 00 00> popIntoTemp: 0 inVectorAt: 0'.
	self assert: (bytecode at: (index := index + 1)) = '53 <00> pushRcvr: 0'.
	self assert: (bytecode at: (index := index + 1)) = '54 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '55 <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes 59 to 62'.
	self assert: (bytecode at: (index := index + 1)) = '59 <8C 00 00> pushTemp: 0 inVectorAt: 0'.
	self assert: (bytecode at: (index := index + 1)) = '62 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '63 <E0> send: add:'.
	self assert: (bytecode at: (index := index + 1)) = '64 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '65 <68> popIntoTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '66 <11> pushTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '67 <8F 10 00 06> closureNumCopied: 1 numArgs: 0 bytes 71 to 76'.
	self assert: (bytecode at: (index := index + 1)) = '71 <8C 00 00> pushTemp: 0 inVectorAt: 0'.
	self assert: (bytecode at: (index := index + 1)) = '74 <21> pushConstant: 5'.
	self assert: (bytecode at: (index := index + 1)) = '75 <B2> send: <'.
	self assert: (bytecode at: (index := index + 1)) = '76 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '77 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '78 <E2> send: whileTrue:'.
	self assert: (bytecode at: (index := index + 1)) = '79 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '80 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '81 <23> pushConstant: #(5 5 5 5 5)'.
	self assert: (bytecode at: (index := index + 1)) = '82 <E4> send: assertValues:'.
	self assert: (bytecode at: (index := index + 1)) = '83 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '84 <78> returnSelf'.

	
	

