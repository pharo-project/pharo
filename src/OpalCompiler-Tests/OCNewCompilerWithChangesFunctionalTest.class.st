Class {
	#name : #OCNewCompilerWithChangesFunctionalTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Misc'
}

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testMethodTemp [
	
	| aCompiledMethod bytecode expected |

	self flag:#oldBytecode.
	self skip.

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

{ #category : #'tests - other' }
OCNewCompilerWithChangesFunctionalTest >> testModulePrimitive [
	
	| aCompiledMethod bytecode expected |

	self flag:#oldBytecode.
	self skip.

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

{ #category : #'tests - array compilation' }
OCNewCompilerWithChangesFunctionalTest >> testMultiElementArray [
	| aCompiledMethod bytecode expected |

	self flag:#oldBytecode.
	self skip.

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

	self flag:#oldBytecode.
	self skip.

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
OCNewCompilerWithChangesFunctionalTest >> testSetUp [
	
	| aCompiledMethod bytecode expected |
	
	self flag:#oldBytecode.
	self skip.	
	
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

{ #category : #'tests - array compilation' }
OCNewCompilerWithChangesFunctionalTest >> testSimpleArray [
	
	| aCompiledMethod bytecode expected |
	
	self flag:#oldBytecode.
	self skip.
	
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
