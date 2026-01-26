Class {
	#name : 'OCIRTransformTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-IR',
	#package : 'OpalCompiler-Tests',
	#tag : 'IR'
}

{ #category : 'tests - adding' }
OCIRTransformTest >> testAdd [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: 1;
		returnTop;
		ir.
	iRMethod allSequences last last delete.
	iRMethod allSequences last last delete.
	iRMethod allSequences last add: (OCIRInstruction pushLiteral: 2).
	iRMethod allSequences last add: OCIRInstruction returnTop.
	aCompiledMethod := iRMethod compiledMethod.
	self
		assert: (aCompiledMethod valueWithReceiver: nil)
		equals: 2
]

{ #category : 'tests - adding' }
OCIRTransformTest >> testAddBefore [
	| iRMethod aCompiledMethod ret |
	iRMethod := OCIRBuilder new
		pushLiteral: 1;
		returnTop;
		ir.
	iRMethod allSequences last last delete.
	iRMethod allSequences last last delete.
	ret := OCIRInstruction returnTop.
	iRMethod allSequences last add: ret.
	iRMethod allSequences last
		add: (OCIRInstruction pushLiteral: 2)
		before: ret.
	aCompiledMethod := iRMethod compiledMethod.
	self
		assert: (aCompiledMethod valueWithReceiver: nil)
		equals: 2
]

{ #category : 'tests - adding' }
OCIRTransformTest >> testAddIntructions [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: 1;
		returnTop;
		ir.
	iRMethod allSequences last last delete.
	iRMethod allSequences last last delete.
	iRMethod allSequences last
		addInstructions:
			{(OCIRInstruction pushLiteral: 2).
			OCIRInstruction returnTop}.
	aCompiledMethod := iRMethod compiledMethod.
	self
		assert: (aCompiledMethod valueWithReceiver: nil)
		equals: 2
]

{ #category : 'tests - adding' }
OCIRTransformTest >> testAddIntructionsBefore [
	| iRMethod aCompiledMethod push |
	iRMethod := OCIRBuilder new
		pushLiteral: 1;
		returnTop;
		ir.
	push := iRMethod allSequences last first.
	iRMethod allSequences last
		addInstructions:
			{(OCIRInstruction pushLiteral: 2).
			OCIRInstruction returnTop}
		before: push.
	aCompiledMethod := iRMethod compiledMethod.
	self
		assert: (aCompiledMethod valueWithReceiver: nil)
		equals: 2
]

{ #category : 'tests - adding' }
OCIRTransformTest >> testAddIntructionsBeforeFromLList [
	| iRMethod aCompiledMethod push llist col |
	iRMethod := OCIRBuilder new
		pushLiteral: 1;
		returnTop;
		ir.
	push := iRMethod allSequences last at: 1.
	llist := LinkedList new.
	llist add: (OCIRInstruction pushLiteral: 2).
	llist add: OCIRInstruction returnTop.
	col := llist asOrderedCollection.
	iRMethod allSequences last addInstructions: col before: push.
	aCompiledMethod := iRMethod compiledMethod.
	self
		assert: (aCompiledMethod valueWithReceiver: nil)
		equals: 2
]

{ #category : 'tests - enumeration' }
OCIRTransformTest >> testAllInstructions [
		| ir |

		ir := OCIRBuilderTest new testPushTempTemp.
		self assert: ir allInstructions last isReturn
]

{ #category : 'tests - enumeration' }
OCIRTransformTest >> testAllTempAccessInstructions [
	| ir |
	ir := OCIRBuilderTest new testPushTempTemp.
	self assert: ir allTempAccessInstructions size equals: 1
]

{ #category : 'tests - adding' }
OCIRTransformTest >> testDelete [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: 1;
		pushLiteral: 2;
		returnTop;
		ir.
	(iRMethod allSequences last
		detect: [ :each | each isPushLiteral: [ :c | c == 2 ] ]) delete.
	aCompiledMethod := iRMethod compiledMethod.
	self
		assert: (aCompiledMethod valueWithReceiver: nil)
		equals: 1
]

{ #category : 'tests - adding' }
OCIRTransformTest >> testReplace [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: 1;
		returnTop;
		ir.
	iRMethod allSequences last first
		replaceWith: (OCIRInstruction pushLiteral: 2).
	aCompiledMethod := iRMethod compiledMethod.
	self
		assert: (aCompiledMethod valueWithReceiver: nil)
		equals: 2
]

{ #category : 'tests - adding' }
OCIRTransformTest >> testReplaceInstr [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: 1;
		returnTop;
		ir.
	iRMethod allSequences last first
		replaceWithInstructions: {(OCIRInstruction pushLiteral: 2)}.
	aCompiledMethod := iRMethod compiledMethod.
	self
		assert: (aCompiledMethod valueWithReceiver: nil)
		equals: 2
]
