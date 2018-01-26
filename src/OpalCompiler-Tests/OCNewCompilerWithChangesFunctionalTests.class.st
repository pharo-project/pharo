Class {
	#name : #OCNewCompilerWithChangesFunctionalTests,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Misc'
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
									
	self testMethod: aCompiledMethod against: '{1} <70> self
{2} <D0> send: announcements
{3} <70> self
{4} <D1> send: index
{5} <8F 00 00 01> closureNumCopied: 0 numArgs: 0 bytes {9} to {9}
{9} <78> returnSelf
{10} <F2> send: at:ifAbsent:
{11} <D3> send: open
{12} <87> pop
{13} <78> returnSelf'.
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
									
	self testMethod: aCompiledMethod against: '{1} <8F 01 00 08> closureNumCopied: 0 numArgs: 1 bytes {5} to {12}
{5} <10> pushTemp: 0
{6} <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes {10} to {11}
{10} <10> pushTemp: 0
{11} <7D> blockReturn
{12} <7D> blockReturn
{13} <68> popIntoTemp: 0
{14} <10> pushTemp: 0
{15} <76> pushConstant: 1
{16} <CA> send: value:
{17} <69> popIntoTemp: 1
{18} <10> pushTemp: 0
{19} <77> pushConstant: 2
{20} <CA> send: value:
{21} <6A> popIntoTemp: 2
{22} <70> self
{23} <11> pushTemp: 1
{24} <C9> send: value
{25} <76> pushConstant: 1
{26} <B6> send: =
{27} <E0> send: assert:
{28} <87> pop
{29} <70> self
{30} <12> pushTemp: 2
{31} <C9> send: value
{32} <77> pushConstant: 2
{33} <B6> send: =
{34} <E0> send: assert:
{35} <87> pop
{36} <78> returnSelf'.
]

{ #category : #'other-tests' }
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
									
self testMethod: aCompiledMethod against: '{1} <8A 01> push: (Array new: 1)
{3} <6A> popIntoTemp: 2
{4} <12> pushTemp: 2
{5} <8F 11 00 0A> closureNumCopied: 1 numArgs: 1 bytes {9} to {18}
{9} <77> pushConstant: 2
{10} <8E 00 01> popIntoTemp: 0 inVectorAt: 1
{13} <8C 00 01> pushTemp: 0 inVectorAt: 1
{16} <10> pushTemp: 0
{17} <B0> send: +
{18} <7D> blockReturn
{19} <68> popIntoTemp: 0
{20} <12> pushTemp: 2
{21} <8F 11 00 06> closureNumCopied: 1 numArgs: 1 bytes {25} to {30}
{25} <8C 00 01> pushTemp: 0 inVectorAt: 1
{28} <10> pushTemp: 0
{29} <B0> send: +
{30} <7D> blockReturn
{31} <69> popIntoTemp: 1
{32} <10> pushTemp: 0
{33} <77> pushConstant: 2
{34} <CA> send: value:
{35} <11> pushTemp: 1
{36} <76> pushConstant: 1
{37} <CA> send: value:
{38} <B0> send: +
{39} <7C> returnTop'.
]

{ #category : #assertions }
OCNewCompilerWithChangesFunctionalTests >> testMethod: cm against: string [

	| symbolic |
	self assert: cm isCompiledMethod.
	symbolic := String streamContents: [:str | cm longPrintOn: str ].

	self assert: symbolic equals: (string format: (cm initialPC to: cm endPC))
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testMethodArgument [
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
		encoderClass: EncoderForV3PlusClosures;
		source:
			'testMethodArgument
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
	self
		testMethod: aCompiledMethod
		against:
			'{1} <8A 01> push: (Array new: 1)
{3} <69> popIntoTemp: 1
{4} <75> pushConstant: 0
{5} <8E 00 01> popIntoTemp: 0 inVectorAt: 1
{8} <11> pushTemp: 1
{9} <8F 10 00 0A> closureNumCopied: 1 numArgs: 0 bytes {13} to {22}
{13} <10> pushTemp: 0
{14} <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes {18} to {21}
{18} <8C 00 00> pushTemp: 0 inVectorAt: 0
{21} <7D> blockReturn
{22} <7D> blockReturn
{23} <68> popIntoTemp: 0
{24} <76> pushConstant: 1
{25} <8E 00 01> popIntoTemp: 0 inVectorAt: 1
{28} <10> pushTemp: 0
{29} <C9> send: value
{30} <68> popIntoTemp: 0
{31} <77> pushConstant: 2
{32} <8E 00 01> popIntoTemp: 0 inVectorAt: 1
{35} <70> self
{36} <10> pushTemp: 0
{37} <C9> send: value
{38} <77> pushConstant: 2
{39} <B6> send: =
{40} <E0> send: assert:
{41} <87> pop
{42} <78> returnSelf'
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testMethodTemp [
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
		encoderClass: EncoderForV3PlusClosures;
		source:
			'testMethodTemp
													| block1 block2 |
													block1 := self methodArgument: 1.
													block2 := self methodArgument: 2.
													self assert: block1 value = 1.
													self assert: block2 value = 2';
		class: OCMockCompilationClass;
		compile.
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self
		testMethod: aCompiledMethod
		against:
			'{1} <70> self
{2} <76> pushConstant: 1
{3} <E0> send: methodArgument:
{4} <68> popIntoTemp: 0
{5} <70> self
{6} <77> pushConstant: 2
{7} <E0> send: methodArgument:
{8} <69> popIntoTemp: 1
{9} <70> self
{10} <10> pushTemp: 0
{11} <C9> send: value
{12} <76> pushConstant: 1
{13} <B6> send: =
{14} <E1> send: assert:
{15} <87> pop
{16} <70> self
{17} <11> pushTemp: 1
{18} <C9> send: value
{19} <77> pushConstant: 2
{20} <B6> send: =
{21} <E1> send: assert:
{22} <87> pop
{23} <78> returnSelf'
]

{ #category : #'other-tests' }
OCNewCompilerWithChangesFunctionalTests >> testModulePrimitive [
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
		encoderClass: EncoderForV3PlusClosures;
		source:
			'copyBitsAgain
	"Primitive. See BitBlt|copyBits, also a Primitive. Essential. See Object
	documentation whatIsAPrimitive."

	<primitive: ''primitiveCopyBits'' module: ''BitBltPlugin''>
	self primitiveFailed';
		class: MockForCompilation;
		compile.
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self
		testMethod: aCompiledMethod
		against:
			'<primitive: ''primitiveCopyBits'' module: ''BitBltPlugin''>
{1} <8B 75 00> callPrimitive: 117
{4} <70> self
{5} <D1> send: primitiveFailed
{6} <87> pop
{7} <78> returnSelf'
]

{ #category : #'test - array compilation' }
OCNewCompilerWithChangesFunctionalTests >> testMultiElementArray [
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
		encoderClass: EncoderForV3PlusClosures;
		source:
			'method
	^{1. 2. 3. 4. 5. 6 }';
		class: OCMockCompilationClass;
		compile.
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self
		testMethod: aCompiledMethod
		against:
			'{1} <76> pushConstant: 1
{2} <77> pushConstant: 2
{3} <20> pushConstant: 3
{4} <21> pushConstant: 4
{5} <22> pushConstant: 5
{6} <23> pushConstant: 6
{7} <8A 86> pop 6 into (Array new: 6)
{9} <7C> returnTop'
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testOneFloat [
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
		encoderClass: EncoderForV3PlusClosures;
		source:
			'one
	
	^1.0';
		class: Float class;
		compile.
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self
		testMethod: aCompiledMethod
		against:
			'{1} <20> pushConstant: 1.0
{2} <7C> returnTop'
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
		encoderClass: EncoderForV3PlusClosures;
		source:
			'methodArgument: anObject
														^ [ anObject ]';
		class: OCMockCompilationClass;
		compile.
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self
		testMethod: aCompiledMethod
		against:
			'{1} <10> pushTemp: 0
{2} <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes {6} to {7}
{6} <10> pushTemp: 0
{7} <7D> blockReturn
{8} <7C> returnTop'
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testSetUp [
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
		encoderClass: EncoderForV3PlusClosures;
		source:
			'setUp
												super setUp.
												collection := OrderedCollection new';
		class: OCMockCompilationClass;
		compile.
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self
		testMethod: aCompiledMethod
		against:
			'{1} <70> self
{2} <85 00> superSend: setUp
{4} <87> pop
{5} <41> pushLit: OrderedCollection
{6} <CC> send: new
{7} <60> popIntoRcvr: 0
{8} <78> returnSelf'
]

{ #category : #'test - array compilation' }
OCNewCompilerWithChangesFunctionalTests >> testSimpleArray [
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
		encoderClass: EncoderForV3PlusClosures;
		source:
			'method
	^{1}';
		class: OCMockCompilationClass;
		compile.
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self
		testMethod: aCompiledMethod
		against:
			'{1} <76> pushConstant: 1
{2} <8A 81> pop 1 into (Array new: 1)
{4} <7C> returnTop'
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testSteamContentsLimitedToSequenceableCollectionClass [
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
		encoderClass: EncoderForV3PlusClosures;
		source:
			'streamContents: blockWithArg limitedTo: sizeLimit
												| stream |
												stream := LimitedWriteStream on: (self new: (100 min: sizeLimit)).
												stream setLimit: sizeLimit limitBlock: [^ stream contents].
												blockWithArg value: stream.
												^ stream contents';
		class: SequenceableCollection class;
		compile.
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self
		testMethod: aCompiledMethod
		against:
			'{1} <40> pushLit: LimitedWriteStream
{2} <70> self
{3} <21> pushConstant: 100
{4} <11> pushTemp: 1
{5} <E2> send: min:
{6} <CD> send: new:
{7} <E3> send: on:
{8} <6A> popIntoTemp: 2
{9} <12> pushTemp: 2
{10} <11> pushTemp: 1
{11} <12> pushTemp: 2
{12} <8F 10 00 03> closureNumCopied: 1 numArgs: 0 bytes {16} to {18}
{16} <10> pushTemp: 0
{17} <D4> send: contents
{18} <7C> returnTop
{19} <F5> send: setLimit:limitBlock:
{20} <87> pop
{21} <10> pushTemp: 0
{22} <12> pushTemp: 2
{23} <CA> send: value:
{24} <87> pop
{25} <12> pushTemp: 2
{26} <D4> send: contents
{27} <7C> returnTop'
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testToDoArgumentNotInlined [
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
		encoderClass: EncoderForV3PlusClosures;
		source:
			'testToDoArgumentNotInlined
													| block |
													block := [ :index |
														collection add: [ index ] ].
													1 to: 5 do: block.
													self assertValues: #(1 2 3 4 5)';
		class: OCMockCompilationClass;
		compile.
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self
		testMethod: aCompiledMethod
		against:
			'{1} <8F 01 00 0A> closureNumCopied: 0 numArgs: 1 bytes {5} to {14}
{5} <00> pushRcvr: 0
{6} <10> pushTemp: 0
{7} <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes {11} to {12}
{11} <10> pushTemp: 0
{12} <7D> blockReturn
{13} <E0> send: add:
{14} <7D> blockReturn
{15} <68> popIntoTemp: 0
{16} <76> pushConstant: 1
{17} <21> pushConstant: 5
{18} <10> pushTemp: 0
{19} <F2> send: to:do:
{20} <87> pop
{21} <70> self
{22} <23> pushConstant: #(1 2 3 4 5)
{23} <E4> send: assertValues:
{24} <87> pop
{25} <78> returnSelf'
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testToDoInsideTempNotInlined [
	"Some instructions are the same but we have a different number at he begining, and the storeTemp and popIntoTemp issue"

	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
		encoderClass: EncoderForV3PlusClosures;
		source:
			'testToDoInsideTempNotInlined
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
	self
		testMethod: aCompiledMethod
		against:
			'{1} <8F 01 00 0D> closureNumCopied: 0 numArgs: 1 bytes {5} to {17}
{5} <73> pushConstant: nil
{6} <10> pushTemp: 0
{7} <69> popIntoTemp: 1
{8} <00> pushRcvr: 0
{9} <11> pushTemp: 1
{10} <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes {14} to {15}
{14} <10> pushTemp: 0
{15} <7D> blockReturn
{16} <E0> send: add:
{17} <7D> blockReturn
{18} <68> popIntoTemp: 0
{19} <76> pushConstant: 1
{20} <21> pushConstant: 5
{21} <10> pushTemp: 0
{22} <F2> send: to:do:
{23} <87> pop
{24} <70> self
{25} <23> pushConstant: #(1 2 3 4 5)
{26} <E4> send: assertValues:
{27} <87> pop
{28} <78> returnSelf'
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testToDoOutsideTempNotInlined [
	"there seems to be a better indexzation of the temps we have one more"

	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
		encoderClass: EncoderForV3PlusClosures;
		source:
			'testToDoOutsideTempNotInlined
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
	self
		testMethod: aCompiledMethod
		against:
			'{1} <8A 01> push: (Array new: 1)
{3} <69> popIntoTemp: 1
{4} <11> pushTemp: 1
{5} <8F 11 00 10> closureNumCopied: 1 numArgs: 1 bytes {9} to {24}
{9} <10> pushTemp: 0
{10} <8E 00 01> popIntoTemp: 0 inVectorAt: 1
{13} <00> pushRcvr: 0
{14} <11> pushTemp: 1
{15} <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes {19} to {22}
{19} <8C 00 00> pushTemp: 0 inVectorAt: 0
{22} <7D> blockReturn
{23} <E0> send: add:
{24} <7D> blockReturn
{25} <68> popIntoTemp: 0
{26} <76> pushConstant: 1
{27} <21> pushConstant: 5
{28} <10> pushTemp: 0
{29} <F2> send: to:do:
{30} <87> pop
{31} <70> self
{32} <23> pushConstant: #(5 5 5 5 5)
{33} <E4> send: assertValues:
{34} <87> pop
{35} <78> returnSelf'
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testWhileModificationAfterNotInlined [
	"The bytecodes integers are different in some cases."

	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
		encoderClass: EncoderForV3PlusClosures;
		source:
			'testWhileModificationAfterNotInlined
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
	self
		testMethod: aCompiledMethod
		against:
			'{1} <8A 01> push: (Array new: 1)
{3} <69> popIntoTemp: 1
{4} <75> pushConstant: 0
{5} <8E 00 01> popIntoTemp: 0 inVectorAt: 1
{8} <11> pushTemp: 1
{9} <8F 10 00 15> closureNumCopied: 1 numArgs: 0 bytes {13} to {33}
{13} <00> pushRcvr: 0
{14} <10> pushTemp: 0
{15} <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes {19} to {22}
{19} <8C 00 00> pushTemp: 0 inVectorAt: 0
{22} <7D> blockReturn
{23} <E0> send: add:
{24} <87> pop
{25} <8C 00 00> pushTemp: 0 inVectorAt: 0
{28} <76> pushConstant: 1
{29} <B0> send: +
{30} <8D 00 00> storeIntoTemp: 0 inVectorAt: 0
{33} <7D> blockReturn
{34} <68> popIntoTemp: 0
{35} <11> pushTemp: 1
{36} <8F 10 00 06> closureNumCopied: 1 numArgs: 0 bytes {40} to {45}
{40} <8C 00 00> pushTemp: 0 inVectorAt: 0
{43} <21> pushConstant: 5
{44} <B2> send: <
{45} <7D> blockReturn
{46} <10> pushTemp: 0
{47} <E2> send: whileTrue:
{48} <87> pop
{49} <70> self
{50} <23> pushConstant: #(5 5 5 5 5)
{51} <E4> send: assertValues:
{52} <87> pop
{53} <78> returnSelf'
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testWhileModificationBeforeNotInlined [
	"The bytecodes integers are different in some cases."

	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
		encoderClass: EncoderForV3PlusClosures;
		source:
			'testWhileModificationBeforeNotInlined
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
	self
		testMethod: aCompiledMethod
		against:
			'{1} <8A 01> push: (Array new: 1)
{3} <69> popIntoTemp: 1
{4} <75> pushConstant: 0
{5} <8E 00 01> popIntoTemp: 0 inVectorAt: 1
{8} <11> pushTemp: 1
{9} <8F 10 00 14> closureNumCopied: 1 numArgs: 0 bytes {13} to {32}
{13} <8C 00 00> pushTemp: 0 inVectorAt: 0
{16} <76> pushConstant: 1
{17} <B0> send: +
{18} <8E 00 00> popIntoTemp: 0 inVectorAt: 0
{21} <00> pushRcvr: 0
{22} <10> pushTemp: 0
{23} <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes {27} to {30}
{27} <8C 00 00> pushTemp: 0 inVectorAt: 0
{30} <7D> blockReturn
{31} <E0> send: add:
{32} <7D> blockReturn
{33} <68> popIntoTemp: 0
{34} <11> pushTemp: 1
{35} <8F 10 00 06> closureNumCopied: 1 numArgs: 0 bytes {39} to {44}
{39} <8C 00 00> pushTemp: 0 inVectorAt: 0
{42} <21> pushConstant: 5
{43} <B2> send: <
{44} <7D> blockReturn
{45} <10> pushTemp: 0
{46} <E2> send: whileTrue:
{47} <87> pop
{48} <70> self
{49} <23> pushConstant: #(5 5 5 5 5)
{50} <E4> send: assertValues:
{51} <87> pop
{52} <78> returnSelf'
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTests >> testWhileWithTempNotInlined [
	"The bytecodes integers are different in some cases."

	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
		encoderClass: EncoderForV3PlusClosures;
		source:
			'testWhileWithTempNotInlined
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
	self
		testMethod: aCompiledMethod
		against:
			'{1} <8A 01> push: (Array new: 1)
{3} <69> popIntoTemp: 1
{4} <75> pushConstant: 0
{5} <8E 00 01> popIntoTemp: 0 inVectorAt: 1
{8} <11> pushTemp: 1
{9} <8F 10 00 14> closureNumCopied: 1 numArgs: 0 bytes {13} to {32}
{13} <73> pushConstant: nil
{14} <8C 00 00> pushTemp: 0 inVectorAt: 0
{17} <76> pushConstant: 1
{18} <B0> send: +
{19} <8D 00 00> storeIntoTemp: 0 inVectorAt: 0
{22} <69> popIntoTemp: 1
{23} <00> pushRcvr: 0
{24} <11> pushTemp: 1
{25} <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes {29} to {30}
{29} <10> pushTemp: 0
{30} <7D> blockReturn
{31} <E0> send: add:
{32} <7D> blockReturn
{33} <68> popIntoTemp: 0
{34} <11> pushTemp: 1
{35} <8F 10 00 06> closureNumCopied: 1 numArgs: 0 bytes {39} to {44}
{39} <8C 00 00> pushTemp: 0 inVectorAt: 0
{42} <21> pushConstant: 5
{43} <B2> send: <
{44} <7D> blockReturn
{45} <10> pushTemp: 0
{46} <E2> send: whileTrue:
{47} <87> pop
{48} <70> self
{49} <23> pushConstant: #(1 2 3 4 5)
{50} <E4> send: assertValues:
{51} <87> pop
{52} <78> returnSelf'
]
