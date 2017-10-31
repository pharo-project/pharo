tests
testBlockTemp
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
							encoderClass:  EncoderForV3PlusClosures; 
									source: 'testBlockTemp
	| block block1 block2 |
	block := [ :arg | [ arg ] ].
	block1 := block value: 1.
	block2 := block value: 2.
	self assert: block1 value = 1.
	self assert: block2 value = 2';
									class: OCMockCompilationClass;
									compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '17 <8F 01 00 08> closureNumCopied: 0 numArgs: 1 bytes 21 to 28'.
self assert: (bytecode at: (index := index + 1)) = '21 <10> pushTemp: 0'.
self assert: (bytecode at: (index := index + 1)) = '22 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 26 to 27'.
self assert: (bytecode at: (index := index + 1)) = '26 <10> pushTemp: 0'.
self assert: (bytecode at: (index := index + 1)) = '27 <7D> blockReturn'.
self assert: (bytecode at: (index := index + 1)) = '28 <7D> blockReturn'.
self assert: (bytecode at: (index := index + 1)) = '29 <68> popIntoTemp: 0'.
self assert: (bytecode at: (index := index + 1)) = '30 <10> pushTemp: 0'.
self assert: (bytecode at: (index := index + 1)) = '31 <76> pushConstant: 1'.
self assert: (bytecode at: (index := index + 1)) = '32 <CA> send: value:'.
self assert: (bytecode at: (index := index + 1)) = '33 <69> popIntoTemp: 1'.
self assert: (bytecode at: (index := index + 1)) = '34 <10> pushTemp: 0'.
self assert: (bytecode at: (index := index + 1)) = '35 <77> pushConstant: 2'.
self assert: (bytecode at: (index := index + 1)) = '36 <CA> send: value:'.
self assert: (bytecode at: (index := index + 1)) = '37 <6A> popIntoTemp: 2'.
self assert: (bytecode at: (index := index + 1)) = '38 <70> self'.
self assert: (bytecode at: (index := index + 1)) = '39 <11> pushTemp: 1'.
self assert: (bytecode at: (index := index + 1)) = '40 <C9> send: value'.
self assert: (bytecode at: (index := index + 1)) = '41 <76> pushConstant: 1'.
self assert: (bytecode at: (index := index + 1)) = '42 <B6> send: ='.
self assert: (bytecode at: (index := index + 1)) = '43 <E0> send: assert:'.
self assert: (bytecode at: (index := index + 1)) = '44 <87> pop'.
self assert: (bytecode at: (index := index + 1)) = '45 <70> self'.
self assert: (bytecode at: (index := index + 1)) = '46 <12> pushTemp: 2'.
self assert: (bytecode at: (index := index + 1)) = '47 <C9> send: value'.
self assert: (bytecode at: (index := index + 1)) = '48 <77> pushConstant: 2'.
self assert: (bytecode at: (index := index + 1)) = '49 <B6> send: ='.
self assert: (bytecode at: (index := index + 1)) = '50 <E0> send: assert:'.
self assert: (bytecode at: (index := index + 1)) = '51 <87> pop'.
self assert: (bytecode at: (index := index + 1)) = '52 <78> returnSelf'.

