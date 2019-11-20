Class {
	#name : #OCNewCompilerWithChangesFunctionalTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Misc'
}

{ #category : #'test - block returning' }
OCNewCompilerWithChangesFunctionalTest >> testBlockReturning [
	
	| aCompiledMethod bytecode expected |
	aCompiledMethod := OpalCompiler new
									encoderClass:  EncoderForV3PlusClosures; 
									source: 'open
	(self announcements at: self index ifAbsent: [ ^ self ]) 
		open';
									class: OCMockCompilationClass;
									compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.

	expected := 	Smalltalk vm 
		for32bit: '29 <70> self
30 <D0> send: announcements
31 <70> self
32 <D1> send: index
33 <8F 00 00 01> closureNumCopied: 0 numArgs: 0 bytes 37 to 37
37 <78> returnSelf
38 <F2> send: at:ifAbsent:
39 <D3> send: open
40 <87> pop
41 <78> returnSelf' 
		for64bit: '57 <70> self
58 <D0> send: announcements
59 <70> self
60 <D1> send: index
61 <8F 00 00 01> closureNumCopied: 0 numArgs: 0 bytes 65 to 65
65 <78> returnSelf
66 <F2> send: at:ifAbsent:
67 <D3> send: open
68 <87> pop
69 <78> returnSelf'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].


]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testBlockTemp [
	
	| aCompiledMethod bytecode expected |
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
	expected := 	Smalltalk vm 
		for32bit: '17 <8F 01 00 08> closureNumCopied: 0 numArgs: 1 bytes 21 to 28
21 <10> pushTemp: 0
22 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 26 to 27
26 <10> pushTemp: 0
27 <7D> blockReturn
28 <7D> blockReturn
29 <68> popIntoTemp: 0
30 <10> pushTemp: 0
31 <76> pushConstant: 1
32 <CA> send: value:
33 <69> popIntoTemp: 1
34 <10> pushTemp: 0
35 <77> pushConstant: 2
36 <CA> send: value:
37 <6A> popIntoTemp: 2
38 <70> self
39 <11> pushTemp: 1
40 <C9> send: value
41 <76> pushConstant: 1
42 <B6> send: =
43 <E0> send: assert:
44 <87> pop
45 <70> self
46 <12> pushTemp: 2
47 <C9> send: value
48 <77> pushConstant: 2
49 <B6> send: =
50 <E0> send: assert:
51 <87> pop
52 <78> returnSelf'
		for64bit: '33 <8F 01 00 08> closureNumCopied: 0 numArgs: 1 bytes 37 to 44
37 <10> pushTemp: 0
38 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 42 to 43
42 <10> pushTemp: 0
43 <7D> blockReturn
44 <7D> blockReturn
45 <68> popIntoTemp: 0
46 <10> pushTemp: 0
47 <76> pushConstant: 1
48 <CA> send: value:
49 <69> popIntoTemp: 1
50 <10> pushTemp: 0
51 <77> pushConstant: 2
52 <CA> send: value:
53 <6A> popIntoTemp: 2
54 <70> self
55 <11> pushTemp: 1
56 <C9> send: value
57 <76> pushConstant: 1
58 <B6> send: =
59 <E0> send: assert:
60 <87> pop
61 <70> self
62 <12> pushTemp: 2
63 <C9> send: value
64 <77> pushConstant: 2
65 <B6> send: =
66 <E0> send: assert:
67 <87> pop
68 <78> returnSelf'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].


]

{ #category : #'other-tests' }
OCNewCompilerWithChangesFunctionalTest >> testDifferentBlocksWithSameArgumentName [
	
	| aCompiledMethod bytecode expected |
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
	expected := 	Smalltalk vm 
		for32bit: '13 <8A 01> push: (Array new: 1)
15 <6A> popIntoTemp: 2
16 <12> pushTemp: 2
17 <8F 11 00 0A> closureNumCopied: 1 numArgs: 1 bytes 21 to 30
21 <77> pushConstant: 2
22 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
25 <8C 00 01> pushTemp: 0 inVectorAt: 1
28 <10> pushTemp: 0
29 <B0> send: +
30 <7D> blockReturn
31 <68> popIntoTemp: 0
32 <12> pushTemp: 2
33 <8F 11 00 06> closureNumCopied: 1 numArgs: 1 bytes 37 to 42
37 <8C 00 01> pushTemp: 0 inVectorAt: 1
40 <10> pushTemp: 0
41 <B0> send: +
42 <7D> blockReturn
43 <69> popIntoTemp: 1
44 <10> pushTemp: 0
45 <77> pushConstant: 2
46 <CA> send: value:
47 <11> pushTemp: 1
48 <76> pushConstant: 1
49 <CA> send: value:
50 <B0> send: +
51 <7C> returnTop'	
	for64bit: '25 <8A 01> push: (Array new: 1)
27 <6A> popIntoTemp: 2
28 <12> pushTemp: 2
29 <8F 11 00 0A> closureNumCopied: 1 numArgs: 1 bytes 33 to 42
33 <77> pushConstant: 2
34 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
37 <8C 00 01> pushTemp: 0 inVectorAt: 1
40 <10> pushTemp: 0
41 <B0> send: +
42 <7D> blockReturn
43 <68> popIntoTemp: 0
44 <12> pushTemp: 2
45 <8F 11 00 06> closureNumCopied: 1 numArgs: 1 bytes 49 to 54
49 <8C 00 01> pushTemp: 0 inVectorAt: 1
52 <10> pushTemp: 0
53 <B0> send: +
54 <7D> blockReturn
55 <69> popIntoTemp: 1
56 <10> pushTemp: 0
57 <77> pushConstant: 2
58 <CA> send: value:
59 <11> pushTemp: 1
60 <76> pushConstant: 1
61 <CA> send: value:
62 <B0> send: +
63 <7C> returnTop'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].

]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testMethodArgument [
	
	| aCompiledMethod bytecode expected |
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
	expected := 	Smalltalk vm 
		for32bit: '17 <8A 01> push: (Array new: 1)
19 <69> popIntoTemp: 1
20 <75> pushConstant: 0
21 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
24 <11> pushTemp: 1
25 <8F 10 00 0A> closureNumCopied: 1 numArgs: 0 bytes 29 to 38
29 <10> pushTemp: 0
30 <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes 34 to 37
34 <8C 00 00> pushTemp: 0 inVectorAt: 0
37 <7D> blockReturn
38 <7D> blockReturn
39 <68> popIntoTemp: 0
40 <76> pushConstant: 1
41 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
44 <10> pushTemp: 0
45 <C9> send: value
46 <68> popIntoTemp: 0
47 <77> pushConstant: 2
48 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
51 <70> self
52 <10> pushTemp: 0
53 <C9> send: value
54 <77> pushConstant: 2
55 <B6> send: =
56 <E0> send: assert:
57 <87> pop
58 <78> returnSelf'
		for64bit: '33 <8A 01> push: (Array new: 1)
35 <69> popIntoTemp: 1
36 <75> pushConstant: 0
37 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
40 <11> pushTemp: 1
41 <8F 10 00 0A> closureNumCopied: 1 numArgs: 0 bytes 45 to 54
45 <10> pushTemp: 0
46 <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes 50 to 53
50 <8C 00 00> pushTemp: 0 inVectorAt: 0
53 <7D> blockReturn
54 <7D> blockReturn
55 <68> popIntoTemp: 0
56 <76> pushConstant: 1
57 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
60 <10> pushTemp: 0
61 <C9> send: value
62 <68> popIntoTemp: 0
63 <77> pushConstant: 2
64 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
67 <70> self
68 <10> pushTemp: 0
69 <C9> send: value
70 <77> pushConstant: 2
71 <B6> send: =
72 <E0> send: assert:
73 <87> pop
74 <78> returnSelf'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].

]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testMethodTemp [
	
	| aCompiledMethod bytecode expected |
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
	expected := 	Smalltalk vm 
		for32bit: '21 <70> self
22 <76> pushConstant: 1
23 <E0> send: methodArgument:
24 <68> popIntoTemp: 0
25 <70> self
26 <77> pushConstant: 2
27 <E0> send: methodArgument:
28 <69> popIntoTemp: 1
29 <70> self
30 <10> pushTemp: 0
31 <C9> send: value
32 <76> pushConstant: 1
33 <B6> send: =
34 <E1> send: assert:
35 <87> pop
36 <70> self
37 <11> pushTemp: 1
38 <C9> send: value
39 <77> pushConstant: 2
40 <B6> send: =
41 <E1> send: assert:
42 <87> pop
43 <78> returnSelf'
		for64bit: '41 <70> self
42 <76> pushConstant: 1
43 <E0> send: methodArgument:
44 <68> popIntoTemp: 0
45 <70> self
46 <77> pushConstant: 2
47 <E0> send: methodArgument:
48 <69> popIntoTemp: 1
49 <70> self
50 <10> pushTemp: 0
51 <C9> send: value
52 <76> pushConstant: 1
53 <B6> send: =
54 <E1> send: assert:
55 <87> pop
56 <70> self
57 <11> pushTemp: 1
58 <C9> send: value
59 <77> pushConstant: 2
60 <B6> send: =
61 <E1> send: assert:
62 <87> pop
63 <78> returnSelf'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].

]

{ #category : #'other-tests' }
OCNewCompilerWithChangesFunctionalTest >> testModulePrimitive [
	
	| aCompiledMethod bytecode expected |
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
	expected := 	Smalltalk vm 
		for32bit: '<primitive: ''primitiveCopyBits'' module: ''BitBltPlugin''>
21 <8B 75 00> callPrimitive: 117
24 <70> self
25 <D1> send: primitiveFailed
26 <87> pop
27 <78> returnSelf'
		for64bit: '<primitive: ''primitiveCopyBits'' module: ''BitBltPlugin''>
41 <8B 75 00> callPrimitive: 117
44 <70> self
45 <D1> send: primitiveFailed
46 <87> pop
47 <78> returnSelf'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].

]

{ #category : #'test - array compilation' }
OCNewCompilerWithChangesFunctionalTest >> testMultiElementArray [
	| aCompiledMethod bytecode expected |
	aCompiledMethod := OpalCompiler new 
									encoderClass:  EncoderForV3PlusClosures; 
									source: 'method
	^{1. 2. 3. 4. 5. 6 }';
									class: OCMockCompilationClass;
									compile.
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	expected := 	Smalltalk vm 
		for32bit: '29 <76> pushConstant: 1
30 <77> pushConstant: 2
31 <20> pushConstant: 3
32 <21> pushConstant: 4
33 <22> pushConstant: 5
34 <23> pushConstant: 6
35 <8A 86> pop 6 into (Array new: 6)
37 <7C> returnTop'
		for64bit: '57 <76> pushConstant: 1
58 <77> pushConstant: 2
59 <20> pushConstant: 3
60 <21> pushConstant: 4
61 <22> pushConstant: 5
62 <23> pushConstant: 6
63 <8A 86> pop 6 into (Array new: 6)
65 <7C> returnTop'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].

]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testOneFloat [
	
	| aCompiledMethod bytecode expected |
	aCompiledMethod := OpalCompiler new 
		encoderClass:  EncoderForV3PlusClosures; 
	source: 'one
	
	^1.0';
	class: Float class;
	compile.

									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	expected := 	Smalltalk vm 
		for32bit: '17 <20> pushConstant: 1.0
18 <7C> returnTop'
		for64bit: '33 <20> pushConstant: 1.0
34 <7C> returnTop'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testPragma [
	| aCompiledMethod bytecode |
	aCompiledMethod := OpalCompiler new
		source:
			'methodDoublePragma
											<hello: 5>
											<hello: 2>';
		class: OCMockCompilationClass;
		compile.

	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	self assert: bytecode first equals: 'Quick return self'
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testReturnBlockInMethod [
	
	| aCompiledMethod bytecode expected |
	aCompiledMethod := OpalCompiler new
											encoderClass:  EncoderForV3PlusClosures; 
											source: 'methodArgument: anObject
														^ [ anObject ]';
											class: OCMockCompilationClass;
											compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	expected := 	Smalltalk vm 
		for32bit: '13 <10> pushTemp: 0
14 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 18 to 19
18 <10> pushTemp: 0
19 <7D> blockReturn
20 <7C> returnTop'
		for64bit: '25 <10> pushTemp: 0
26 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 30 to 31
30 <10> pushTemp: 0
31 <7D> blockReturn
32 <7C> returnTop'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].


]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testSetUp [
	
	| aCompiledMethod bytecode expected |
	aCompiledMethod := OpalCompiler new
									encoderClass:  EncoderForV3PlusClosures; 
									source: 'setUp
												super setUp.
												collection := OrderedCollection new';
									class: OCMockCompilationClass;
									compile.
	
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	expected := 	Smalltalk vm 
		for32bit: '21 <70> self
22 <85 00> superSend: setUp
24 <87> pop
25 <41> pushLit: OrderedCollection
26 <CC> send: new
27 <60> popIntoRcvr: 0
28 <78> returnSelf'
		for64bit: '41 <70> self
42 <85 00> superSend: setUp
44 <87> pop
45 <41> pushLit: OrderedCollection
46 <CC> send: new
47 <60> popIntoRcvr: 0
48 <78> returnSelf'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].

]

{ #category : #'test - array compilation' }
OCNewCompilerWithChangesFunctionalTest >> testSimpleArray [
	
	| aCompiledMethod bytecode expected |
	aCompiledMethod := OpalCompiler new
									encoderClass:  EncoderForV3PlusClosures; 
									source: 'method
	^{1}';
									class: OCMockCompilationClass;
									compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	expected := 	Smalltalk vm 
		for32bit: '13 <76> pushConstant: 1
14 <8A 81> pop 1 into (Array new: 1)
16 <7C> returnTop'
		for64bit: '25 <76> pushConstant: 1
26 <8A 81> pop 1 into (Array new: 1)
28 <7C> returnTop'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testSteamContentsLimitedToSequenceableCollectionClass [
	| aCompiledMethod bytecode expected |
	
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
	expected := 	Smalltalk vm 
		for32bit: '37 <40> pushLit: LimitedWriteStream
38 <70> self
39 <21> pushConstant: 100
40 <11> pushTemp: 1
41 <E2> send: min:
42 <CD> send: new:
43 <E3> send: on:
44 <6A> popIntoTemp: 2
45 <12> pushTemp: 2
46 <11> pushTemp: 1
47 <12> pushTemp: 2
48 <8F 10 00 03> closureNumCopied: 1 numArgs: 0 bytes 52 to 54
52 <10> pushTemp: 0
53 <D4> send: contents
54 <7C> returnTop
55 <F5> send: setLimit:limitBlock:
56 <87> pop
57 <10> pushTemp: 0
58 <12> pushTemp: 2
59 <CA> send: value:
60 <87> pop
61 <12> pushTemp: 2
62 <D4> send: contents
63 <7C> returnTop'	
		for64bit: '73 <40> pushLit: LimitedWriteStream
74 <70> self
75 <21> pushConstant: 100
76 <11> pushTemp: 1
77 <E2> send: min:
78 <CD> send: new:
79 <E3> send: on:
80 <6A> popIntoTemp: 2
81 <12> pushTemp: 2
82 <11> pushTemp: 1
83 <12> pushTemp: 2
84 <8F 10 00 03> closureNumCopied: 1 numArgs: 0 bytes 88 to 90
88 <10> pushTemp: 0
89 <D4> send: contents
90 <7C> returnTop
91 <F5> send: setLimit:limitBlock:
92 <87> pop
93 <10> pushTemp: 0
94 <12> pushTemp: 2
95 <CA> send: value:
96 <87> pop
97 <12> pushTemp: 2
98 <D4> send: contents
99 <7C> returnTop'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testToDoArgumentNotInlined [
	
	| aCompiledMethod bytecode expected |
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
	expected := 	Smalltalk vm 
		for32bit: '33 <8F 01 00 0A> closureNumCopied: 0 numArgs: 1 bytes 37 to 46
37 <00> pushRcvr: 0
38 <10> pushTemp: 0
39 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 43 to 44
43 <10> pushTemp: 0
44 <7D> blockReturn
45 <E0> send: add:
46 <7D> blockReturn
47 <68> popIntoTemp: 0
48 <76> pushConstant: 1
49 <21> pushConstant: 5
50 <10> pushTemp: 0
51 <F2> send: to:do:
52 <87> pop
53 <70> self
54 <23> pushConstant: #(1 2 3 4 5)
55 <E4> send: assertValues:
56 <87> pop
57 <78> returnSelf'
		for64bit: '65 <8F 01 00 0A> closureNumCopied: 0 numArgs: 1 bytes 69 to 78
69 <00> pushRcvr: 0
70 <10> pushTemp: 0
71 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 75 to 76
75 <10> pushTemp: 0
76 <7D> blockReturn
77 <E0> send: add:
78 <7D> blockReturn
79 <68> popIntoTemp: 0
80 <76> pushConstant: 1
81 <21> pushConstant: 5
82 <10> pushTemp: 0
83 <F2> send: to:do:
84 <87> pop
85 <70> self
86 <23> pushConstant: #(1 2 3 4 5)
87 <E4> send: assertValues:
88 <87> pop
89 <78> returnSelf'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testToDoInsideTempNotInlined [
	"Some instructions are the same but we have a different number at he begining, and the storeTemp and popIntoTemp issue"
	| aCompiledMethod bytecode expected |
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
	expected := 	Smalltalk vm 
		for32bit: 
'33 <8F 01 00 0D> closureNumCopied: 0 numArgs: 1 bytes 37 to 49
37 <73> pushConstant: nil
38 <10> pushTemp: 0
39 <69> popIntoTemp: 1
40 <00> pushRcvr: 0
41 <11> pushTemp: 1
42 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 46 to 47
46 <10> pushTemp: 0
47 <7D> blockReturn
48 <E0> send: add:
49 <7D> blockReturn
50 <68> popIntoTemp: 0
51 <76> pushConstant: 1
52 <21> pushConstant: 5
53 <10> pushTemp: 0
54 <F2> send: to:do:
55 <87> pop
56 <70> self
57 <23> pushConstant: #(1 2 3 4 5)
58 <E4> send: assertValues:
59 <87> pop
60 <78> returnSelf'
		for64bit: '65 <8F 01 00 0D> closureNumCopied: 0 numArgs: 1 bytes 69 to 81
69 <73> pushConstant: nil
70 <10> pushTemp: 0
71 <69> popIntoTemp: 1
72 <00> pushRcvr: 0
73 <11> pushTemp: 1
74 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 78 to 79
78 <10> pushTemp: 0
79 <7D> blockReturn
80 <E0> send: add:
81 <7D> blockReturn
82 <68> popIntoTemp: 0
83 <76> pushConstant: 1
84 <21> pushConstant: 5
85 <10> pushTemp: 0
86 <F2> send: to:do:
87 <87> pop
88 <70> self
89 <23> pushConstant: #(1 2 3 4 5)
90 <E4> send: assertValues:
91 <87> pop
92 <78> returnSelf'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testToDoOutsideTempNotInlined [
	"there seems to be a better indexzation of the temps we have one more"
	| aCompiledMethod bytecode expected |
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
	expected := 	Smalltalk vm 
		for32bit: '33 <8A 01> push: (Array new: 1)
35 <69> popIntoTemp: 1
36 <11> pushTemp: 1
37 <8F 11 00 10> closureNumCopied: 1 numArgs: 1 bytes 41 to 56
41 <10> pushTemp: 0
42 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
45 <00> pushRcvr: 0
46 <11> pushTemp: 1
47 <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes 51 to 54
51 <8C 00 00> pushTemp: 0 inVectorAt: 0
54 <7D> blockReturn
55 <E0> send: add:
56 <7D> blockReturn
57 <68> popIntoTemp: 0
58 <76> pushConstant: 1
59 <21> pushConstant: 5
60 <10> pushTemp: 0
61 <F2> send: to:do:
62 <87> pop
63 <70> self
64 <23> pushConstant: #(5 5 5 5 5)
65 <E4> send: assertValues:
66 <87> pop
67 <78> returnSelf'
		for64bit: '65 <8A 01> push: (Array new: 1)
67 <69> popIntoTemp: 1
68 <11> pushTemp: 1
69 <8F 11 00 10> closureNumCopied: 1 numArgs: 1 bytes 73 to 88
73 <10> pushTemp: 0
74 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
77 <00> pushRcvr: 0
78 <11> pushTemp: 1
79 <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes 83 to 86
83 <8C 00 00> pushTemp: 0 inVectorAt: 0
86 <7D> blockReturn
87 <E0> send: add:
88 <7D> blockReturn
89 <68> popIntoTemp: 0
90 <76> pushConstant: 1
91 <21> pushConstant: 5
92 <10> pushTemp: 0
93 <F2> send: to:do:
94 <87> pop
95 <70> self
96 <23> pushConstant: #(5 5 5 5 5)
97 <E4> send: assertValues:
98 <87> pop
99 <78> returnSelf'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].


]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testWhileModificationAfterNotInlined [
	"The bytecodes integers are different in some cases."
	| aCompiledMethod bytecode expected |
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
	expected := 	Smalltalk vm 
		for32bit: '33 <8A 01> push: (Array new: 1)
35 <69> popIntoTemp: 1
36 <75> pushConstant: 0
37 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
40 <11> pushTemp: 1
41 <8F 10 00 15> closureNumCopied: 1 numArgs: 0 bytes 45 to 65
45 <00> pushRcvr: 0
46 <10> pushTemp: 0
47 <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes 51 to 54
51 <8C 00 00> pushTemp: 0 inVectorAt: 0
54 <7D> blockReturn
55 <E0> send: add:
56 <87> pop
57 <8C 00 00> pushTemp: 0 inVectorAt: 0
60 <76> pushConstant: 1
61 <B0> send: +
62 <8D 00 00> storeIntoTemp: 0 inVectorAt: 0
65 <7D> blockReturn
66 <68> popIntoTemp: 0
67 <11> pushTemp: 1
68 <8F 10 00 06> closureNumCopied: 1 numArgs: 0 bytes 72 to 77
72 <8C 00 00> pushTemp: 0 inVectorAt: 0
75 <21> pushConstant: 5
76 <B2> send: <
77 <7D> blockReturn
78 <10> pushTemp: 0
79 <E2> send: whileTrue:
80 <87> pop
81 <70> self
82 <23> pushConstant: #(5 5 5 5 5)
83 <E4> send: assertValues:
84 <87> pop
85 <78> returnSelf'
		for64bit: '65 <8A 01> push: (Array new: 1)
67 <69> popIntoTemp: 1
68 <75> pushConstant: 0
69 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
72 <11> pushTemp: 1
73 <8F 10 00 15> closureNumCopied: 1 numArgs: 0 bytes 77 to 97
77 <00> pushRcvr: 0
78 <10> pushTemp: 0
79 <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes 83 to 86
83 <8C 00 00> pushTemp: 0 inVectorAt: 0
86 <7D> blockReturn
87 <E0> send: add:
88 <87> pop
89 <8C 00 00> pushTemp: 0 inVectorAt: 0
92 <76> pushConstant: 1
93 <B0> send: +
94 <8D 00 00> storeIntoTemp: 0 inVectorAt: 0
97 <7D> blockReturn
98 <68> popIntoTemp: 0
99 <11> pushTemp: 1
100 <8F 10 00 06> closureNumCopied: 1 numArgs: 0 bytes 104 to 109
104 <8C 00 00> pushTemp: 0 inVectorAt: 0
107 <21> pushConstant: 5
108 <B2> send: <
109 <7D> blockReturn
110 <10> pushTemp: 0
111 <E2> send: whileTrue:
112 <87> pop
113 <70> self
114 <23> pushConstant: #(5 5 5 5 5)
115 <E4> send: assertValues:
116 <87> pop
117 <78> returnSelf'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].
]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testWhileModificationBeforeNotInlined [
	"The bytecodes integers are different in some cases."
	| aCompiledMethod bytecode expected |
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
	expected := 	Smalltalk vm 
		for32bit: '33 <8A 01> push: (Array new: 1)
35 <69> popIntoTemp: 1
36 <75> pushConstant: 0
37 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
40 <11> pushTemp: 1
41 <8F 10 00 14> closureNumCopied: 1 numArgs: 0 bytes 45 to 64
45 <8C 00 00> pushTemp: 0 inVectorAt: 0
48 <76> pushConstant: 1
49 <B0> send: +
50 <8E 00 00> popIntoTemp: 0 inVectorAt: 0
53 <00> pushRcvr: 0
54 <10> pushTemp: 0
55 <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes 59 to 62
59 <8C 00 00> pushTemp: 0 inVectorAt: 0
62 <7D> blockReturn
63 <E0> send: add:
64 <7D> blockReturn
65 <68> popIntoTemp: 0
66 <11> pushTemp: 1
67 <8F 10 00 06> closureNumCopied: 1 numArgs: 0 bytes 71 to 76
71 <8C 00 00> pushTemp: 0 inVectorAt: 0
74 <21> pushConstant: 5
75 <B2> send: <
76 <7D> blockReturn
77 <10> pushTemp: 0
78 <E2> send: whileTrue:
79 <87> pop
80 <70> self
81 <23> pushConstant: #(5 5 5 5 5)
82 <E4> send: assertValues:
83 <87> pop
84 <78> returnSelf'
		for64bit: '65 <8A 01> push: (Array new: 1)
67 <69> popIntoTemp: 1
68 <75> pushConstant: 0
69 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
72 <11> pushTemp: 1
73 <8F 10 00 14> closureNumCopied: 1 numArgs: 0 bytes 77 to 96
77 <8C 00 00> pushTemp: 0 inVectorAt: 0
80 <76> pushConstant: 1
81 <B0> send: +
82 <8E 00 00> popIntoTemp: 0 inVectorAt: 0
85 <00> pushRcvr: 0
86 <10> pushTemp: 0
87 <8F 10 00 04> closureNumCopied: 1 numArgs: 0 bytes 91 to 94
91 <8C 00 00> pushTemp: 0 inVectorAt: 0
94 <7D> blockReturn
95 <E0> send: add:
96 <7D> blockReturn
97 <68> popIntoTemp: 0
98 <11> pushTemp: 1
99 <8F 10 00 06> closureNumCopied: 1 numArgs: 0 bytes 103 to 108
103 <8C 00 00> pushTemp: 0 inVectorAt: 0
106 <21> pushConstant: 5
107 <B2> send: <
108 <7D> blockReturn
109 <10> pushTemp: 0
110 <E2> send: whileTrue:
111 <87> pop
112 <70> self
113 <23> pushConstant: #(5 5 5 5 5)
114 <E4> send: assertValues:
115 <87> pop
116 <78> returnSelf'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].

]

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testWhileWithTempNotInlined [
	"The bytecodes integers are different in some cases."
	| aCompiledMethod bytecode expected |
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
	expected := 	Smalltalk vm 
		for32bit: '33 <8A 01> push: (Array new: 1)
35 <69> popIntoTemp: 1
36 <75> pushConstant: 0
37 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
40 <11> pushTemp: 1
41 <8F 10 00 14> closureNumCopied: 1 numArgs: 0 bytes 45 to 64
45 <73> pushConstant: nil
46 <8C 00 00> pushTemp: 0 inVectorAt: 0
49 <76> pushConstant: 1
50 <B0> send: +
51 <8D 00 00> storeIntoTemp: 0 inVectorAt: 0
54 <69> popIntoTemp: 1
55 <00> pushRcvr: 0
56 <11> pushTemp: 1
57 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 61 to 62
61 <10> pushTemp: 0
62 <7D> blockReturn
63 <E0> send: add:
64 <7D> blockReturn
65 <68> popIntoTemp: 0
66 <11> pushTemp: 1
67 <8F 10 00 06> closureNumCopied: 1 numArgs: 0 bytes 71 to 76
71 <8C 00 00> pushTemp: 0 inVectorAt: 0
74 <21> pushConstant: 5
75 <B2> send: <
76 <7D> blockReturn
77 <10> pushTemp: 0
78 <E2> send: whileTrue:
79 <87> pop
80 <70> self
81 <23> pushConstant: #(1 2 3 4 5)
82 <E4> send: assertValues:
83 <87> pop
84 <78> returnSelf'
		for64bit: '65 <8A 01> push: (Array new: 1)
67 <69> popIntoTemp: 1
68 <75> pushConstant: 0
69 <8E 00 01> popIntoTemp: 0 inVectorAt: 1
72 <11> pushTemp: 1
73 <8F 10 00 14> closureNumCopied: 1 numArgs: 0 bytes 77 to 96
77 <73> pushConstant: nil
78 <8C 00 00> pushTemp: 0 inVectorAt: 0
81 <76> pushConstant: 1
82 <B0> send: +
83 <8D 00 00> storeIntoTemp: 0 inVectorAt: 0
86 <69> popIntoTemp: 1
87 <00> pushRcvr: 0
88 <11> pushTemp: 1
89 <8F 10 00 02> closureNumCopied: 1 numArgs: 0 bytes 93 to 94
93 <10> pushTemp: 0
94 <7D> blockReturn
95 <E0> send: add:
96 <7D> blockReturn
97 <68> popIntoTemp: 0
98 <11> pushTemp: 1
99 <8F 10 00 06> closureNumCopied: 1 numArgs: 0 bytes 103 to 108
103 <8C 00 00> pushTemp: 0 inVectorAt: 0
106 <21> pushConstant: 5
107 <B2> send: <
108 <7D> blockReturn
109 <10> pushTemp: 0
110 <E2> send: whileTrue:
111 <87> pop
112 <70> self
113 <23> pushConstant: #(1 2 3 4 5)
114 <E4> send: assertValues:
115 <87> pop
116 <78> returnSelf'.

	expected lines with: bytecode do: [ :line :aBytecode | self assert: line equals: aBytecode ].
]
