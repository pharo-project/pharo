Class {
	#name : #OCBytecodeGeneratorTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Bytecode'
}

{ #category : #accessing }
OCBytecodeGeneratorTest class >> packageNamesUnderTest [
	^ #('OpalCompiler')
]

{ #category : #helper }
OCBytecodeGeneratorTest >> newBytecodeGen [
	^ IRBytecodeGenerator newWithEncoderClass: EncoderForV3PlusClosures
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testBlockReturnTop [
	| cm |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		blockReturnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <7D> blockReturn' 
		for64bit: '25 <7D> blockReturn' )
	.
	
]

{ #category : #'tests - execution' }
OCBytecodeGeneratorTest >> testClosureExample [
	| cm |
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushClosureCopyNumCopiedValues: 0 numArgs: 0 to: #end;
		pushReceiver;
		blockReturnTop;
		label: #end;
		returnTop;
		compiledMethod.


	cm methodClass: self class.
	cm selector: #test.

	self assert: (cm isKindOf: CompiledMethod).
	self assert: ((cm valueWithReceiver: 2 @ 2 arguments: #()) isKindOf: BlockClosure).
	self assert: (cm valueWithReceiver: 2 @ 2 arguments: #()) value equals: 2 @ 2.
	^ cm
]

{ #category : #'tests - execution' }
OCBytecodeGeneratorTest >> testExample [
	| cm |
	cm := self newBytecodeGen
		numArgs: 1;
		numTemps: 1;
		pushTemp: 1;
		pushInstVar: 2;
		send: #>;
		if: false goto: #else;
		pushLiteral: 'yes';
		returnTop;
		label: #else;
		pushLiteral: 'no';
		returnTop;
		compiledMethod.
	cm methodClass: self class.
	cm selector: #test.

	self assert: (cm isKindOf: CompiledMethod).
	self assert: (cm valueWithReceiver: 2 @ 2 arguments: #(1)) equals: 'no'.
	self assert: (cm valueWithReceiver: 2 @ 2 arguments: #(3)) equals: 'yes'.
	^ cm
]

{ #category : #'tests - execution' }
OCBytecodeGeneratorTest >> testExample2 [
	| cm |
	cm := self newBytecodeGen
		numArgs: 1;
		numTemps: 1;
		pushInstVar: 2;
		pushTemp: 1;
		send: #<;
		if: false goto: #else;
		pushLiteral: 'yes';
		returnTop;
		label: #else;
		pushLiteral: 'no';
		returnTop;
		compiledMethod.

	self assert: (cm isKindOf: CompiledMethod).
	self assert: (cm valueWithReceiver: 2 @ 2 arguments: #(1)) equals: 'no'.
	self assert: (cm valueWithReceiver: 2 @ 2 arguments: #(3)) equals: 'yes'.
	^ cm
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testGoTo [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		label: #back;
		pushLiteral: 1;
		returnTop;
		goto: #back;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <76> pushConstant: 1\14 <76> pushConstant: 1\15 <7C> returnTop\16 <A3 FC> jumpTo: 14' withCRs 
		for64bit: '25 <76> pushConstant: 1\26 <76> pushConstant: 1\27 <7C> returnTop\28 <A3 FC> jumpTo: 26' withCRs)

]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testGoToIf [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 1;
		pushTemp: 1;
		pushLiteral: 1;
		if: true goto: #return;
		returnTop;
		label: #return;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <10> pushTemp: 0\14 <76> pushConstant: 1\15 <A8 01> jumpTrue: 18\17 <7C> returnTop\18 <7C> returnTop' withCRs 
		for64bit: '25 <10> pushTemp: 0\26 <76> pushConstant: 1\27 <A8 01> jumpTrue: 30\29 <7C> returnTop\30 <7C> returnTop' withCRs)
	
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testIfGoTo [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 1;
		pushTemp: 1;
		pushLiteral: 1;
		if: true goto: #return;
		returnTop;
		label: #return;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <10> pushTemp: 0\14 <76> pushConstant: 1\15 <A8 01> jumpTrue: 18\17 <7C> returnTop\18 <7C> returnTop' withCRs 
		for64bit: '25 <10> pushTemp: 0\26 <76> pushConstant: 1\27 <A8 01> jumpTrue: 30\29 <7C> returnTop\30 <7C> returnTop' withCRs)

]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testIfGoToOtherWise [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 1;
		pushTemp: 1;
		pushLiteral: 1;
		if: true goto: #return otherwise: #other;
		label: #other;
		returnTop;
		label: #return;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <10> pushTemp: 0\14 <76> pushConstant: 1\15 <A8 01> jumpTrue: 18\17 <7C> returnTop\18 <7C> returnTop' withCRs 
		for64bit: '25 <10> pushTemp: 0\26 <76> pushConstant: 1\27 <A8 01> jumpTrue: 30\29 <7C> returnTop\30 <7C> returnTop' withCRs)
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testLabel [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		label: #back;
		pushLiteral: 1;
		returnTop;
		goto: #back;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <76> pushConstant: 1\14 <76> pushConstant: 1\15 <7C> returnTop\16 <A3 FC> jumpTo: 14' withCRs 
		for64bit: '25 <76> pushConstant: 1\26 <76> pushConstant: 1\27 <7C> returnTop\28 <A3 FC> jumpTo: 26' withCRs)
]

{ #category : #helper }
OCBytecodeGeneratorTest >> testMethod: cm against: string [
	| symbolic |
	self assert: cm isCompiledMethod.
	symbolic := String streamContents: [ :str | cm longPrintOn: str ].
	self assert: symbolic equals: string
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testPopTop [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		pushLiteral: 1;
		popTop;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <76> pushConstant: 1\14 <76> pushConstant: 1\15 <87> pop\16 <7C> returnTop' withCRs 
		for64bit: '25 <76> pushConstant: 1\26 <76> pushConstant: 1\27 <87> pop\28 <7C> returnTop' withCRs)
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testPushClosureCopyNumCopiedValuesNumArgsTo [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushClosureCopyNumCopiedValues: 0 numArgs: 0 to: #end;
		pushReceiver;
		blockReturnTop;
		label: #end;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <8F 00 00 02> closureNumCopied: 0 numArgs: 0 bytes 17 to 18\17 <70> self\18 <7D> blockReturn\19 <7C> returnTop' withCRs 
		for64bit: '25 <8F 00 00 02> closureNumCopied: 0 numArgs: 0 bytes 29 to 30\29 <70> self\30 <7D> blockReturn\31 <7C> returnTop' withCRs)  
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testPushConsArray [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		pushConsArray: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <76> pushConstant: 1\14 <8A 81> pop 1 into (Array new: 1)\16 <7C> returnTop' withCRs 
		for64bit: '25 <76> pushConstant: 1\26 <8A 81> pop 1 into (Array new: 1)\28 <7C> returnTop' withCRs) 
	
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testPushDup [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushDup;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <88> dup\14 <7C> returnTop' withCRs 
		for64bit: '25 <88> dup\26 <7C> returnTop' withCRs) 	
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testPushInstVar [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushInstVar: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <00> pushRcvr: 0\14 <7C> returnTop' withCRs 
		for64bit: '25 <00> pushRcvr: 0\26 <7C> returnTop' withCRs) 
	
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testPushLiteral [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <76> pushConstant: 1\14 <7C> returnTop' withCRs 
		for64bit: '25 <76> pushConstant: 1\26 <7C> returnTop' withCRs) 
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testPushLiteralVariable [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteralVariable: (AdditionalBinding key: '1');
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '17 <40> pushLit: 1\18 <7C> returnTop' withCRs 
		for64bit: '33 <40> pushLit: 1\34 <7C> returnTop' withCRs) 	
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testPushNewArray [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushNewArray: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <8A 01> push: (Array new: 1)\15 <7C> returnTop' withCRs 
		for64bit: '25 <8A 01> push: (Array new: 1)\27 <7C> returnTop' withCRs) 
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testPushReceiver [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushReceiver;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <70> self\14 <7C> returnTop' withCRs 
		for64bit: '25 <70> self\26 <7C> returnTop' withCRs) 
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testPushRemoteTempInVectorAt [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushRemoteTemp: 1 inVectorAt: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <8C 00 00> pushTemp: 0 inVectorAt: 0\16 <7C> returnTop' withCRs 
		for64bit: '25 <8C 00 00> pushTemp: 0 inVectorAt: 0\28 <7C> returnTop' withCRs)
]

{ #category : #'tests - errors' }
OCBytecodeGeneratorTest >> testPushRemoteTempInVectorAtOutOfRange [
	
	self should: [ self newBytecodeGen
					numArgs: 0;
					pushRemoteTemp: 1 inVectorAt: 257 ] 
		raise: Error 
	
]

{ #category : #'tests - errors' }
OCBytecodeGeneratorTest >> testPushRemoteTempOutOfRangeInVectorAt [
	
	self should: [ self newBytecodeGen
					numArgs: 0;
					pushRemoteTemp: 257 inVectorAt: 1 ] 
		raise: Error 
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testPushTemp [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 1;
		pushTemp: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <10> pushTemp: 0\14 <7C> returnTop' withCRs 
		for64bit: '25 <10> pushTemp: 0\26 <7C> returnTop' withCRs) 
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testPushThisContext [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushThisContext ;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <89> pushThisContext\14 <7C> returnTop' withCRs 
		for64bit: '25 <89> pushThisContext\26 <7C> returnTop' withCRs) 
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testReturnConstant [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		returnConstant: 1;
		compiledMethod.

	self testMethod: cm against:  'Quick return 1'
	
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testReturnInstVar [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		returnInstVar: 1;
		compiledMethod.

	self testMethod: cm against:  'Quick return field 0 (0-based)'
	
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testReturnReceiver [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		returnReceiver;
		compiledMethod.

	self testMethod: cm against:  'Quick return self'
	
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testReturnTop [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <76> pushConstant: 1\14 <7C> returnTop' withCRs 
		for64bit: '25 <76> pushConstant: 1\26 <7C> returnTop' withCRs)
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testSend [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		send: #a;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '17 <76> pushConstant: 1\18 <D0> send: a\19 <7C> returnTop' withCRs 
		for64bit: '33 <76> pushConstant: 1\34 <D0> send: a\35 <7C> returnTop' withCRs) 
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testSendToSuperOf [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		send: #a toSuperOf: Object;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '17 <76> pushConstant: 1\18 <85 00> superSend: a\20 <7C> returnTop' withCRs 
		for64bit: '33 <76> pushConstant: 1\34 <85 00> superSend: a\36 <7C> returnTop' withCRs)
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testStoreInstVar [
	| gen cm symbolic |
	gen := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		storeInstVar: 1;
		returnTop.
	cm := gen compiledMethod.
	symbolic := String streamContents: [ :str | cm longPrintOn: str ].

	self assert: gen stackFrameSize equals: 1.
	self assert: cm isCompiledMethod.
	self
		assert: symbolic
		equals:
			(Smalltalk vm
				for32bit: '13 <76> pushConstant: 1\14 <81 00> storeIntoRcvr: 0\16 <7C> returnTop' withCRs
				for64bit: '25 <76> pushConstant: 1\26 <81 00> storeIntoRcvr: 0\28 <7C> returnTop' withCRs)
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testStoreIntoLiteralVariable [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		storeIntoLiteralVariable: (LookupKey key: '1');
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '17 <76> pushConstant: 1\18 <81 C0> storeIntoLit: 1\20 <7C> returnTop' withCRs 
		for64bit: '33 <76> pushConstant: 1\34 <81 C0> storeIntoLit: 1\36 <7C> returnTop' withCRs)
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testStoreLiteralVariable [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		storeIntoLiteralVariable: OCBytecodeGeneratorTest binding;
		returnTop;
		compiledMethod.
	self testMethod: cm against: (Smalltalk vm 
		for32bit: '17 <76> pushConstant: 1\18 <81 C0> storeIntoLit: OCBytecodeGeneratorTest\20 <7C> returnTop' withCRs 
		for64bit: '33 <76> pushConstant: 1\34 <81 C0> storeIntoLit: OCBytecodeGeneratorTest\36 <7C> returnTop' withCRs)
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testStorePopInstVar [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		pushLiteral: 1;
		storePopInstVar: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <76> pushConstant: 1\14 <76> pushConstant: 1\15 <60> popIntoRcvr: 0\16 <7C> returnTop' withCRs 
		for64bit: '25 <76> pushConstant: 1\26 <76> pushConstant: 1\27 <60> popIntoRcvr: 0\28 <7C> returnTop' withCRs)
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testStorePopIntoLiteralVariable [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		pushLiteral: 1;
		storePopIntoLiteralVariable: (#test->1);
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '17 <76> pushConstant: 1\18 <76> pushConstant: 1\19 <82 C0> popIntoLit: test\21 <7C> returnTop' withCRs 
		for64bit: '33 <76> pushConstant: 1\34 <76> pushConstant: 1\35 <82 C0> popIntoLit: test\37 <7C> returnTop' withCRs)
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testStorePopRemoteTempInVectorAt [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		pushLiteral: 1;
		storePopRemoteTemp: 1 inVectorAt: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <76> pushConstant: 1\14 <76> pushConstant: 1\15 <8E 00 00> popIntoTemp: 0 inVectorAt: 0\18 <7C> returnTop' withCRs 
		for64bit: '25 <76> pushConstant: 1\26 <76> pushConstant: 1\27 <8E 00 00> popIntoTemp: 0 inVectorAt: 0\30 <7C> returnTop' withCRs)
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testStorePopTemp [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		pushLiteral: 1;
		storePopTemp: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <76> pushConstant: 1\14 <76> pushConstant: 1\15 <68> popIntoTemp: 0\16 <7C> returnTop' withCRs 
		for64bit: '25 <76> pushConstant: 1\26 <76> pushConstant: 1\27 <68> popIntoTemp: 0\28 <7C> returnTop' withCRs)
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testStoreRemoteTempInVectorAt [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		storeRemoteTemp: 1 inVectorAt: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <76> pushConstant: 1\14 <8D 00 00> storeIntoTemp: 0 inVectorAt: 0\17 <7C> returnTop' withCRs 
		for64bit: '25 <76> pushConstant: 1\26 <8D 00 00> storeIntoTemp: 0 inVectorAt: 0\29 <7C> returnTop' withCRs)
]

{ #category : #'tests - errors' }
OCBytecodeGeneratorTest >> testStoreRemoteTempInVectorAtOutOfRange [
	
	self should: [ self newBytecodeGen
					numArgs: 0;
					storeRemoteTemp: 1 inVectorAt: 257 ] 
		raise: Error 
]

{ #category : #'tests - errors' }
OCBytecodeGeneratorTest >> testStoreRemoteTempOutOfRangeInVectorAt [
	
	self should: [ self newBytecodeGen
					numArgs: 0;
					storeRemoteTemp: 257 inVectorAt: 1 ] 
		raise: Error 
]

{ #category : #'tests - instructions' }
OCBytecodeGeneratorTest >> testStoreTemp [
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 1;
		pushLiteral: 1;
		storeTemp: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against: (Smalltalk vm 
		for32bit: '13 <76> pushConstant: 1\14 <81 40> storeIntoTemp: 0\16 <7C> returnTop' withCRs 
		for64bit: '25 <76> pushConstant: 1\26 <81 40> storeIntoTemp: 0\28 <7C> returnTop' withCRs)
]
