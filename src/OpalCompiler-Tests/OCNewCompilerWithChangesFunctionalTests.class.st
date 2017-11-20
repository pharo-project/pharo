Class {
	#name : #OCNewCompilerWithChangesFunctionalTests,
	#superclass : #TestCase,
	#category : #OpalCompiler-Tests-Misc
}

{ #category : #'test - block returning' }
OCNewCompilerWithChangesFunctionalTests >> testBlockReturning [
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
									encoderClass:  EncoderForV3PlusClosures; 
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



]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testBlockTemp [
	
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


]

{ #category : #other-tests }
OCNewCompilerWithChangesFunctionalTests >> testDifferentBlocksWithSameArgumentName [
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
									encoderClass:  EncoderForV3PlusClosures; 
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
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testMethodArgument [
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new 
											encoderClass:  EncoderForV3PlusClosures; 
											source: 'testMethodArgument
															| temp block |
															temp := 0.
															block := [ [ temp ] ].
															temp := 1.
															block := block value.
															temp := 2.
															self assert: block value = 2';
											class: OCMockCompilationClass;
											compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '17 <8A 01> push: (Array new: 1)'.
	self assert: (bytecode at: (index := index + 1)) = '19 <69> popIntoTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '20 <75> pushConstant: 0'.
	self assert: (bytecode at: (index := index + 1)) = '21 <8E 00 01> popIntoTemp: 0 inVectorAt: 1'.
	self assert: (bytecode at: (index := index + 1)) = '24 <11> pushTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '25 <8F 10 00 0A> closureNumCopied: 1 numArgs: 0 bytes 29 to 38'.
	self assert: (bytecode at: (index := index + 1)) = '29 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '30 <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes 34 to 37'.
	self assert: (bytecode at: (index := index + 1)) = '34 <8C 00 00> pushTemp: 0 inVectorAt: 0'.
	self assert: (bytecode at: (index := index + 1)) = '37 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '38 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '39 <68> popIntoTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '40 <76> pushConstant: 1'.
	self assert: (bytecode at: (index := index + 1)) = '41 <8E 00 01> popIntoTemp: 0 inVectorAt: 1'.
	self assert: (bytecode at: (index := index + 1)) = '44 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '45 <C9> send: value'.
	self assert: (bytecode at: (index := index + 1)) = '46 <68> popIntoTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '47 <77> pushConstant: 2'.
	self assert: (bytecode at: (index := index + 1)) = '48 <8E 00 01> popIntoTemp: 0 inVectorAt: 1'.
	self assert: (bytecode at: (index := index + 1)) = '51 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '52 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '53 <C9> send: value'.
	self assert: (bytecode at: (index := index + 1)) = '54 <77> pushConstant: 2'.
	self assert: (bytecode at: (index := index + 1)) = '55 <B6> send: ='.
	self assert: (bytecode at: (index := index + 1)) = '56 <E0> send: assert:'.
	self assert: (bytecode at: (index := index + 1)) = '57 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '58 <78> returnSelf'.



]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testMethodTemp [
	
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


]

{ #category : #other-tests }
OCNewCompilerWithChangesFunctionalTests >> testModulePrimitive [
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new 
	encoderClass:  EncoderForV3PlusClosures; 
	source: 'copyBitsAgain
	"Primitive. See BitBlt|copyBits, also a Primitive. Essential. See Object
	documentation whatIsAPrimitive."

	<primitive: ''primitiveCopyBits'' module: ''BitBltPlugin''>
	self primitiveFailed';
	class: MockForCompilation;
	compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '<primitive: ''primitiveCopyBits'' module: ''BitBltPlugin''>'.
	self assert: (bytecode at: (index := index + 1)) = '21 <8B 75 00> callPrimitive: 117'.
	self assert: (bytecode at: (index := index + 1)) = '24 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '25 <D1> send: primitiveFailed'.
	self assert: (bytecode at: (index := index + 1)) = '26 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '27 <78> returnSelf'.	

	
]

{ #category : #'test - array compilation' }
OCNewCompilerWithChangesFunctionalTests >> testMultiElementArray [
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new 
									encoderClass:  EncoderForV3PlusClosures; 
									source: 'method
	^{1. 2. 3. 4. 5. 6 }';
									class: OCMockCompilationClass;
									compile.
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '29 <76> pushConstant: 1'.
	self assert: (bytecode at: (index := index + 1)) = '30 <77> pushConstant: 2'.
	self assert: (bytecode at: (index := index + 1)) = '31 <20> pushConstant: 3'.
	self assert: (bytecode at: (index := index + 1)) = '32 <21> pushConstant: 4'.
	self assert: (bytecode at: (index := index + 1)) = '33 <22> pushConstant: 5'.
	self assert: (bytecode at: (index := index + 1)) = '34 <23> pushConstant: 6'.
	self assert: (bytecode at: (index := index + 1)) = '35 <8A 86> pop 6 into (Array new: 6)'.
	self assert: (bytecode at: (index := index + 1)) = '37 <7C> returnTop'.

]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testOneFloat [
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new 
		encoderClass:  EncoderForV3PlusClosures; 
	source: 'one
	
	^1.0';
	class: Float class;
	compile.

									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '17 <20> pushConstant: 1.0'.
	self assert: (bytecode at: (index := index + 1)) = '18 <7C> returnTop'.
	
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testPragma [
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new 
									source: 'methodDoublePragma
											<hello: 5>
											<hello: 2>';
									class:  OCMockCompilationClass;
									compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = 'Quick return self'.


]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testReturnBlockInMethod [
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
											encoderClass:  EncoderForV3PlusClosures; 
											source: 'methodArgument: anObject
														^ [ anObject ]';
											class: OCMockCompilationClass;
											compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '13 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '14 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 18 to 19'.
	self assert: (bytecode at: (index := index + 1)) = '18 <10> pushTemp: 0'.
	self assert: (bytecode at: (index := index + 1)) = '19 <7D> blockReturn'.
	self assert: (bytecode at: (index := index + 1)) = '20 <7C> returnTop'.



]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testSetUp [
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
									encoderClass:  EncoderForV3PlusClosures; 
									source: 'setUp
												super setUp.
												collection := OrderedCollection new';
									class: OCMockCompilationClass;
									compile.
	
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) =  '21 <70> self'.
	self assert: (bytecode at: (index := index + 1)) =  '22 <85 00> superSend: setUp'.
	self assert: (bytecode at: (index := index + 1)) =  '24 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) =  '25 <41> pushLit: OrderedCollection'.
	self assert: (bytecode at: (index := index + 1)) =  '26 <CC> send: new'.
	self assert: (bytecode at: (index := index + 1)) =  '27 <60> popIntoRcvr: 0'.
	self assert: (bytecode at: (index := index + 1)) =  '28 <78> returnSelf'.

]

{ #category : #'test - array compilation' }
OCNewCompilerWithChangesFunctionalTests >> testSimpleArray [
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
									encoderClass:  EncoderForV3PlusClosures; 
									source: 'method
	^{1}';
									class: OCMockCompilationClass;
									compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '13 <76> pushConstant: 1'.
	self assert: (bytecode at: (index := index + 1)) = '14 <8A 81> pop 1 into (Array new: 1)'.
	self assert: (bytecode at: (index := index + 1)) = '16 <7C> returnTop'.

]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testSteamContentsLimitedToSequenceableCollectionClass [
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
	
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testToDoArgumentNotInlined [
	
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

]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testToDoInsideTempNotInlined [
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




]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testToDoOutsideTempNotInlined [
	"there seems to be a better indexzation of the temps we have one more"
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
										encoderClass:  EncoderForV3PlusClosures; 
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

	


]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testWhileModificationAfterNotInlined [
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

	
	


]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testWhileModificationBeforeNotInlined [
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

	
	


]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testWhileWithTempNotInlined [
	"The bytecodes integers are different in some cases."
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
										encoderClass:  EncoderForV3PlusClosures; 
										source: 'testWhileWithTempNotInlined
	| index block |
	index := 0.
	block := [
		| temp |
		temp := index := index + 1.
		collection add: [ temp ] ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(1 2 3 4 5)';
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
	self assert: (bytecode at: (index := index + 1)) = '45 <73> pushConstant: nil'.
	self assert: (bytecode at: (index := index + 1)) = '46 <8C 00 00> pushTemp: 0 inVectorAt: 0'.
	self assert: (bytecode at: (index := index + 1)) = '49 <76> pushConstant: 1'.
	self assert: (bytecode at: (index := index + 1)) = '50 <B0> send: +'.
	self assert: (bytecode at: (index := index + 1)) = '51 <8D 00 00> storeIntoTemp: 0 inVectorAt: 0'.
	self assert: (bytecode at: (index := index + 1)) = '54 <69> popIntoTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '55 <00> pushRcvr: 0'.
	self assert: (bytecode at: (index := index + 1)) = '56 <11> pushTemp: 1'.
	self assert: (bytecode at: (index := index + 1)) = '57 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 61 to 62'.
	self assert: (bytecode at: (index := index + 1)) = '61 <10> pushTemp: 0'.
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
	self assert: (bytecode at: (index := index + 1)) = '81 <23> pushConstant: #(1 2 3 4 5)'.
	self assert: (bytecode at: (index := index + 1)) = '82 <E4> send: assertValues:'.
	self assert: (bytecode at: (index := index + 1)) = '83 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '84 <78> returnSelf'.

	
	


]
