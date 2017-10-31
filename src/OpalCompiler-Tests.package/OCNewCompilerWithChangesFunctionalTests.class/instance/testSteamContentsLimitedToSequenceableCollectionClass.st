tests
testSteamContentsLimitedToSequenceableCollectionClass
	| aCompiledMethod bytecode index |
	
	aCompiledMethod := OpalCompiler new
									encoderClass:  EncoderForV3PlusClosures; 
									source: 'streamContents: blockWithArg limitedTo: sizeLimit
												| stream |
												stream := LimitedWriteStream on: (self new: (100 min: sizeLimit)).
												stream setLimit: sizeLimit limitBlock: [^ stream contents].
												blockWithArg value: stream.
												^ stream contents';
									class: SequenceableCollection class;
									compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '37 <40> pushLit: LimitedWriteStream'.
	self assert: (bytecode at: (index := index + 1)) = '38 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '39 <21> pushConstant: 100'.
	self assert: (bytecode at: (index := index + 1)) = '40 <11> pushTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '41 <E2> send: min:'.
	self assert: (bytecode at: (index := index + 1)) = '42 <CD> send: new:'.
	self assert: (bytecode at: (index := index + 1)) = '43 <E3> send: on:'.
	self assert: (bytecode at: (index := index + 1)) = '44 <6A> popIntoTemp: 2'.
	self assert: (bytecode at: (index := index + 1)) = '45 <12> pushTemp: 2'.
	self assert: (bytecode at: (index := index + 1)) = '46 <11> pushTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '47 <12> pushTemp: 2'.
	self assert: (bytecode at: (index := index + 1)) = '48 <8F 10 00 03> closureNumCopied: 1 numArgs: 0 bytes 52 to 54'.
	self assert: (bytecode at: (index := index + 1)) = '52 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '53 <D4> send: contents'.
	self assert: (bytecode at: (index := index + 1)) = '54 <7C> returnTop'.
	self assert: (bytecode at: (index := index + 1)) = '55 <F5> send: setLimit:limitBlock:'.
	self assert: (bytecode at: (index := index + 1)) = '56 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '57 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '58 <12> pushTemp: 2'.
	self assert: (bytecode at: (index := index + 1)) = '59 <CA> send: value:'.
	self assert: (bytecode at: (index := index + 1)) = '60 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '61 <12> pushTemp: 2'.
	self assert: (bytecode at: (index := index + 1)) = '62 <D4> send: contents'.
	self assert: (bytecode at: (index := index + 1)) = '63 <7C> returnTop'.
	