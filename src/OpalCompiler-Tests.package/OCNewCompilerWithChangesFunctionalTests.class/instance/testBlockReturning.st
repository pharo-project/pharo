test - block returning
testBlockReturning
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
									encoderClass:  OpalEncoderForV3PlusClosures; 
									source: 'open
	(self announcements at: self index ifAbsent: [ ^ self ]) 
		open';
									class: OCMockCompilationClass;
									compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '29 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '30 <D0> send: announcements'.
	self assert: (bytecode at: (index := index + 1)) = '31 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '32 <D1> send: index'.
	self assert: (bytecode at: (index := index + 1)) = '33 <8F 00 00 01> closureNumCopied: 0 numArgs: 0 bytes 37 to 37'.
	self assert: (bytecode at: (index := index + 1)) = '37 <78> returnSelf'.
	self assert: (bytecode at: (index := index + 1)) = '38 <F2> send: at:ifAbsent:'.
	self assert: (bytecode at: (index := index + 1)) = '39 <D3> send: open'.
	self assert: (bytecode at: (index := index + 1)) = '40 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '41 <78> returnSelf'.


