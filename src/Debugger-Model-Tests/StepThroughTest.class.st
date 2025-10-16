Class {
	#name : 'StepThroughTest',
	#superclass : 'DebuggerTest',
	#instVars : [
		'currentFastStepThrough',
		'aBlock',
		'failed'
	],
	#category : 'Debugger-Model-Tests-Core',
	#package : 'Debugger-Model-Tests',
	#tag : 'Core'
}

{ #category : 'helper' }
StepThroughTest >> evalBlock: aBlockClosure afterLoop: remainingCount [

	remainingCount = 1 ifTrue: [ ^ aBlockClosure value ].

	^ self evalBlock: aBlockClosure afterLoop: remainingCount - 1
]

{ #category : 'helper' }
StepThroughTest >> evalBlockThenReturnOne: block [
	block value.
	^1
]

{ #category : 'utilities' }
StepThroughTest >> settingUpSessionAndProcessAndContextForBlock: block [

	currentFastStepThrough := DebugSession fastStepThroughMode.
	DebugSession fastStepThroughMode: false.
	super settingUpSessionAndProcessAndContextForBlock: block
]

{ #category : 'helper' }
StepThroughTest >> stepA1 [
	self evalBlockThenReturnOne: [ self stepA2 ]
]

{ #category : 'helper' }
StepThroughTest >> stepA2 [
	^ 2+2
]

{ #category : 'helper' }
StepThroughTest >> stepB1 [
	self stepB2.
	self stepB3
]

{ #category : 'helper' }
StepThroughTest >> stepB2 [
	^ 42
]

{ #category : 'helper' }
StepThroughTest >> stepB3 [
	^ 43
]

{ #category : 'helper' }
StepThroughTest >> stepC1 [

	self evalBlock: [ self stepC2 ] afterLoop: 10000
]

{ #category : 'helper' }
StepThroughTest >> stepC2 [

	^ 5
]

{ #category : 'helper' }
StepThroughTest >> stepD1 [

	aBlock := [ 2 + 3 ].

	self stepD2
]

{ #category : 'helper' }
StepThroughTest >> stepD2 [

	aBlock value
]

{ #category : 'helper' }
StepThroughTest >> stepE1 [

	| tmpBlock |
	tmpBlock := [ 2 - 3 ].

	self evalBlockThenReturnOne: tmpBlock
]

{ #category : 'helper' }
StepThroughTest >> stepF1 [

	| tmpBlock |
	tmpBlock := [ 2 - 3 ].

	tmpBlock value
]

{ #category : 'helper' }
StepThroughTest >> stepG1 [

	| sem |
	sem := Semaphore new.

	self stepG2: [ 1 + 3 ] sem: sem
]

{ #category : 'helper' }
StepThroughTest >> stepG2: aTmp sem: sem [

	[
		sem wait.
		[
			aTmp value.
			failed := false ]
			on: Exception
			do: [ :e | failed := true ] ] forkAt: Processor activePriority + 1.

	sem signal.

	[ failed isNil ] whileTrue: [ Processor yield ]
]

{ #category : 'helper' }
StepThroughTest >> stepH1 [

	| tmpBlock |
	tmpBlock := [ :a | a + 3 ].

	tmpBlock value: 1
]

{ #category : 'helper' }
StepThroughTest >> stepI1 [

	^ self stepI2: [ :i | i + 1 ]
]

{ #category : 'helper' }
StepThroughTest >> stepI2: someBlock [

	^ someBlock value: 24
]

{ #category : 'running' }
StepThroughTest >> tearDown [

	currentFastStepThrough ifNotNil: [ DebugSession fastStepThroughMode: currentFastStepThrough ].
	super tearDown
]

{ #category : 'tests' }
StepThroughTest >> testStepIntoBlockArgumentInMessageSend [

	| node expectedMethod |
	self settingUpSessionAndProcessAndContextForBlock: [ self stepI1 ].
	session stepInto.
	session stepInto. 

	"Reached node 'self stepI2: [ :i | i + 1 ]' of method stepI1" 
	"Checking that the execution is indeed at this node"
	self assert: session interruptedContext method equals: (self class lookupSelector: #stepI1).
	node := (self class lookupSelector: #stepI1) sourceNodeForPC: session interruptedContext pc.
	self assert: node isMessage.
	self assert: node selector equals: #stepI2:.
	session stepThrough. 
	
	"Reached message node in the blocks body '+ 1'"
	expectedMethod := (self class lookupSelector: #stepI1) literalAt: 1.
	node := expectedMethod compiledBlock sourceNodeForPC: session interruptedContext pc.
	self assert: node isMessage.
	self assert: node selector equals: #+.
	session stepThrough. 
	
	"Reached sequence node in the blocks body 'i + 1'"
	expectedMethod := (self class lookupSelector: #stepI1) literalAt: 1.
	self assert: session interruptedContext method equals: expectedMethod compiledBlock.
	node := expectedMethod compiledBlock sourceNodeForPC: session interruptedContext pc.
	self assert: node isSequence.
	session stepThrough. 
	
	"Reached ^ self stepI2: [:i | i + 1 ] in stepI1"
	self assert: session interruptedContext method equals: (self class lookupSelector: #stepI1).
	node := (self class lookupSelector: #stepI1) sourceNodeForPC: session interruptedContext pc.
	self assert: node isReturn.
	self assert: session interruptedContext sender isNotNil
]

{ #category : 'tests' }
StepThroughTest >> testStepThrough [
	"In a context c, define a block b, send a message to another method to get b evaluated.
	Testing that a step through on this message send moves the execution to the point where the block b is about to be evaluated."
	| node expectedMethod |
	self settingUpSessionAndProcessAndContextForBlock: [ self stepA1 ].
	session stepInto.
	session stepInto.
	"Reached node 'self evalBlockThenReturnOne: [self stepA2]' of method stepA1"
	"Checking that the execution is indeed at this node"
	self assert: (session interruptedContext method) equals: (self class lookupSelector: #stepA1).
	node := (self class lookupSelector: #stepA1) sourceNodeForPC: session interruptedContext pc.
	self assert: node isMessage.
	self assert: node receiver isSelfVariable.
	self assert: node selector equals: #evalBlockThenReturnOne:.
	session stepThrough.

	"With fullblocks the method of the suspended context is a compiledBlock, not the method having it"
	expectedMethod := (self class lookupSelector: #stepA1) literalAt: 1 .

	"Checking that after the step through, the execution is at the 'self stepA2' node of the stepA1 method"
	self assert: (session interruptedContext method) equals: expectedMethod.
	node := expectedMethod sourceNodeForPC: session interruptedContext pc.
	self assert: node isMessage.
	self assert: node receiver isSelfVariable.
	self assert: node selector equals: #stepA2.
		self deny: (session isContextPostMortem: session interruptedContext).
	self assert: session interruptedContext sender isNotNil
	
]

{ #category : 'tests' }
StepThroughTest >> testStepThroughDoesTheSameThingAsStepOverWhenNoBlockIsInvolved [
	"Testing that when no block is involved, a step through moves the execution to the same point a step over would"
	| node |

	self settingUpSessionAndProcessAndContextForBlock: [ self stepB1 ].
	[session interruptedContext method == (self class lookupSelector: #stepB1)]
		whileFalse: [ session stepInto ].

	"Reached node 'self stepB2' of method stepB1"
	self assert: session interruptedContext method equals: (self class lookupSelector: #stepB1).
	session stepOver.
	self assert: (session interruptedContext method) equals: (self class lookupSelector: #stepB1).
	"Checking that after the step over, we reached the node 'self stepB3' of method stepB1"
	node := (self class lookupSelector: #stepB1) sourceNodeForPC: session interruptedContext pc.
	self assert: node isMessage.
	self assert: node receiver isSelfVariable.
	self assert: node selector equals: #stepB3.

	"Set up the debugged execution again"
	self settingUpSessionAndProcessAndContextForBlock: [ self stepB1 ].
	[session interruptedContext method == (self class lookupSelector: #stepB1)]
		whileFalse: [ session stepInto ].

	"Reached node 'self stepB2' of method stepB1"
	self assert: session interruptedContext method equals: (self class lookupSelector: #stepB1).
	session stepThrough.
	"Checking that after the step through, we reached the node 'self stepB3' of method stepB1 (the same node that was reached with the step over"
	node := (self class lookupSelector: #stepB1) sourceNodeForPC: session interruptedContext pc.
	self assert: node isMessage.
	self assert: node receiver isSelfVariable.
	self assert: node selector equals: #stepB3.
	self deny: (session isContextPostMortem: session interruptedContext).
	self assert: session interruptedContext sender isNotNil
]

{ #category : 'tests' }
StepThroughTest >> testStepThroughInABlockInAInstanceVariable [
	"In a context c, define a block b, send a message to another method to get b evaluated.
	Testing that a step through on this message send moves the execution to the point where the block b is about to be evaluated."
	| node expectedMethod |

	self settingUpSessionAndProcessAndContextForBlock: [ self stepD1 ].
	session stepInto.
	session stepInto.
	session stepInto.

	"Reached node 'self evalBlock: [ self stepC2 ] afterLoop: 10' of method stepC1"
	"Checking that the execution is indeed at this node"
	self assert: (session interruptedContext method) equals: (self class lookupSelector: #stepD1).
	node := (self class lookupSelector:#stepD1) sourceNodeForPC: session interruptedContext pc.
	session stepThrough.

	"With fullblocks the method of the suspended context is a compiledBlock, not the method having it"
	expectedMethod := (self class lookupSelector: #stepD1) literalAt: 1 .

	"Checking that after the step through, the execution is at the 'self stepA2' node of the stepA1 method"
	self assert: (session interruptedContext method) equals: expectedMethod compiledBlock.
	node := expectedMethod compiledBlock sourceNodeForPC: session interruptedContext pc.
	self assert: node isMessage.
	self assert: node receiver value equals: 2.
	self assert: node selector equals: #+.
	self deny: (session isContextPostMortem: session interruptedContext).
	self assert: session interruptedContext sender isNotNil
]

{ #category : 'tests' }
StepThroughTest >> testStepThroughInABlockInATemporary [

	| node expectedMethod |

	self settingUpSessionAndProcessAndContextForBlock: [ self stepE1 ].
	session stepInto.
	session stepInto.
	session stepInto.

	self assert: (session interruptedContext method) equals: (self class lookupSelector: #stepE1).
	node := (self class lookupSelector:#stepE1) sourceNodeForPC: session interruptedContext pc.
	session stepThrough.

	"With fullblocks the method of the suspended context is a compiledBlock, not the method having it"
	expectedMethod := (self class lookupSelector: #stepE1) literalAt: 1 .

	"Checking that after the step through, the execution is at the 'self stepA2' node of the stepA1 method"
	self assert: (session interruptedContext method) equals: expectedMethod compiledBlock.
	node := expectedMethod compiledBlock sourceNodeForPC: session interruptedContext pc.
	self assert: node isMessage.
	self assert: node receiver value equals: 2.
	self assert: node selector equals: #-.
	self deny: (session isContextPostMortem: session interruptedContext).
	self assert: session interruptedContext sender isNotNil
]

{ #category : 'tests' }
StepThroughTest >> testStepThroughInABlockInATemporaryDirectly [

	| node expectedMethod |

	self settingUpSessionAndProcessAndContextForBlock: [ self stepF1 ].
	session stepInto.
	session stepInto.
	session stepInto.

	self assert: (session interruptedContext method) equals: (self class lookupSelector: #stepF1).
	node := (self class lookupSelector:#stepF1) sourceNodeForPC: session interruptedContext pc.
	session stepThrough.

	"With fullblocks the method of the suspended context is a compiledBlock, not the method having it"
	expectedMethod := (self class lookupSelector: #stepF1) literalAt: 1 .

	"Checking that after the step through, the execution is at the 'self stepA2' node of the stepA1 method"
	self assert: (session interruptedContext method) equals: expectedMethod compiledBlock.
	node := expectedMethod compiledBlock sourceNodeForPC: session interruptedContext pc.
	self assert: node isMessage.
	self assert: node receiver value equals: 2.
	self assert: node selector equals: #-.
	self deny: (session isContextPostMortem: session interruptedContext).
	self assert: session interruptedContext sender isNotNil
]

{ #category : 'tests' }
StepThroughTest >> testStepThroughInOtherProcess [

	self settingUpSessionAndProcessAndContextForBlock: [ self stepG1 ].
	session stepInto.
	session stepThrough.
	session stepThrough.
	session stepThrough.
	session stepThrough.

	self deny: failed.
	self deny: (session isContextPostMortem: session interruptedContext).
	self assert: session interruptedContext sender isNotNil
]

{ #category : 'tests' }
StepThroughTest >> testStepThroughJumpsToHomeWhenBlockEvaluationEnds [
	"In a context c, define a block b, send a message to another method to get b evaluated.
	Testing that a step through on this message send moves the execution to the point where the block b is about to be evaluated."
	| node expectedMethod |
	
	self settingUpSessionAndProcessAndContextForBlock: [ self stepA1 ].
	session stepInto.
	session stepInto.
	session stepThrough.
	session stepThrough.
	session stepThrough.

	expectedMethod := self class lookupSelector: #stepA1 .

	"Checking that after the step through, the execution is at the 'stepA1' return node"
	self assert: (session interruptedContext method) equals: expectedMethod.
	node := expectedMethod sourceNodeForPC: session interruptedContext pc.
	self assert: node isMethod.
	self assert: node selector equals: #stepA1.
	self deny: (session isContextPostMortem: session interruptedContext).
	self assert: session interruptedContext sender isNotNil
]

{ #category : 'tests' }
StepThroughTest >> testStepThroughLonger [
	"In a context c, define a block b, send a message to another method to get b evaluated.
	Testing that a step through on this message send moves the execution to the point where the block b is about to be evaluated."
	| node expectedMethod |

	self settingUpSessionAndProcessAndContextForBlock: [ self stepC1 ].
	session stepInto.
	session stepInto.
	
	"Reached node 'self evalBlock: [ self stepC2 ] afterLoop: 10' of method stepC1"
	"Checking that the execution is indeed at this node"
	self assert: (session interruptedContext method) equals: (self class lookupSelector:#stepC1).
	node := (self class lookupSelector:#stepC1) sourceNodeForPC: session interruptedContext pc.
	session stepThrough.

	"With fullblocks the method of the suspended context is a compiledBlock, not the method having it"
	expectedMethod := (self class lookupSelector: #stepC1) literalAt: 1 .

	"Checking that after the step through, the execution is at the 'self stepA2' node of the stepA1 method"
	self assert: (session interruptedContext method) equals: expectedMethod.
	node := expectedMethod sourceNodeForPC: session interruptedContext pc.
	self assert: node isMessage.
	self assert: node receiver isSelfVariable.
	self assert: node selector equals: #stepC2.
	self deny: (session isContextPostMortem: session interruptedContext).
	self assert: session interruptedContext sender isNotNil
]

{ #category : 'tests' }
StepThroughTest >> testStepThroughUntilTermination [
	"Stepping over a message node brings the execution to the next node in the same method."

	self settingUpSessionAndProcessAndContextForBlock: [ self stepA1 ].
	[ session interruptedProcess isTerminated ] whileFalse: [ session stepThrough ].

	self assert: session interruptedProcess isTerminated.
	self deny: (session isContextPostMortem: session interruptedContext).
	self assert: session interruptedContext sender isNil
]

{ #category : 'tests' }
StepThroughTest >> testStepThroughValueWithArguments [ 

	| node |
	self settingUpSessionAndProcessAndContextForBlock: [ self stepH1 ].
	session stepInto.
	session stepOver. 
	session stepOver.

	"Reached node 'tmpBlock value: 1' of method stepH1"
	"Checking that the execution is indeed at this node"
	self assert: (session interruptedContext method) equals: (self class lookupSelector: #stepH1).
	node := (self class lookupSelector: #stepH1) sourceNodeForPC: session interruptedContext pc.
	self assert: node isMessage.
	self assert: node selector equals: #value:.
	session stepThrough.

	self assert: session interruptedContext numTemps equals: 1.
	self assert: session interruptedContext sender isNotNil
]
