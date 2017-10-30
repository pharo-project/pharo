tests
testWhileModificationAfterNotInlined
	"The bytecodes integers are different in some cases."
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
										encoderClass:  EncoderForV3PlusClosures; 
										source: 'testWhileModificationAfterNotInlined
														| index block |
														index := 0.
														block := [ 
															collection add: [ index ].
															index := index + 1 ].
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
	self assert: (bytecode at: (index := index + 1)) = '41 <8F 10 00 15> closureNumCopied: 1 numArgs: 0 bytes 45 to 65'.
	self assert: (bytecode at: (index := index + 1)) = '45 <00> pushRcvr: 0'.
	self assert: (bytecode at: (index := index + 1)) = '46 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '47 <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes 51 to 54'.
	self assert: (bytecode at: (index := index + 1)) = '51 <8C 00 00> pushTemp: 0 inVectorAt: 0'.
	self assert: (bytecode at: (index := index + 1)) = '54 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '55 <E0> send: add:'.
	self assert: (bytecode at: (index := index + 1)) = '56 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '57 <8C 00 00> pushTemp: 0 inVectorAt: 0'.
	self assert: (bytecode at: (index := index + 1)) = '60 <76> pushConstant: 1'.
	self assert: (bytecode at: (index := index + 1)) = '61 <B0> send: +'.
	self assert: (bytecode at: (index := index + 1)) = '62 <8D 00 00> storeIntoTemp: 0 inVectorAt: 0'.
	self assert: (bytecode at: (index := index + 1)) = '65 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '66 <68> popIntoTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '67 <11> pushTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '68 <8F 10 00 06> closureNumCopied: 1 numArgs: 0 bytes 72 to 77'.
	self assert: (bytecode at: (index := index + 1)) = '72 <8C 00 00> pushTemp: 0 inVectorAt: 0'.
	self assert: (bytecode at: (index := index + 1)) = '75 <21> pushConstant: 5'.
	self assert: (bytecode at: (index := index + 1)) = '76 <B2> send: <'.
	self assert: (bytecode at: (index := index + 1)) = '77 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '78 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '79 <E2> send: whileTrue:'.
	self assert: (bytecode at: (index := index + 1)) = '80 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '81 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '82 <23> pushConstant: #(5 5 5 5 5)'.
	self assert: (bytecode at: (index := index + 1)) = '83 <E4> send: assertValues:'.
	self assert: (bytecode at: (index := index + 1)) = '84 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '85 <78> returnSelf'.

	
	

