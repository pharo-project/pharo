Extension { #name : 'Context' }

{ #category : '*Debugging-Core' }
Context >> findSecondToOldestSimilarSender [
	"Search the stack for the second-to-oldest occurance of self's method.  Very useful for an infinite recursion.  Gets back to the second call so you can see one complete recursion cycle, and how it was called at the beginning."

	| secondContext context lastContext |
	secondContext := self.
	context := self.
	[	lastContext := context findSimilarSender.
		lastContext isNil
	] whileFalse: [
		secondContext := context.
		context := lastContext.
	].
	^ secondContext
]

{ #category : '*Debugging-Core' }
Context >> findSimilarSender [
	"Return the closest sender with the same method, return nil if none found"

	^ self sender findContextSuchThat: [ :context | context compiledCode == self method ]
]

{ #category : '*Debugging-Core' }
Context >> hasTemporaryVariableNamed: aName [
	^ self temporaryVariableNames includes: aName
]

{ #category : '*Debugging-Core' }
Context >> instructionStream [
	
	^ InstructionStream on: method pc: pc
]

{ #category : '*Debugging-Core' }
Context >> pcRangeContextIsActive: contextIsActive [
	"return the debug highlight for aPC"

	| pcToHighlight |
	"If the context is suspended on a send, we want to highlight the send.
	However, the context already executed the send, and the PC advanced to the next instruction.
	Thus, we want the instruction previously executed!"
	pcToHighlight := contextIsActive
		                 ifTrue: [ self currentPC ]
		                 ifFalse: [ self executedPC ].
	^ self method debugInfo rangeForPC: pcToHighlight
]

{ #category : '*Debugging-Core' }
Context >> respondsToUnknownBytecode [
	"This method is triggerred by the VM when the interpreter tries to execute an unknown bytecode"
	
	| unknownBytecode |
	unknownBytecode := self compiledCode at: self pc.
	self error: 'VM cannot run unknown bytecode ', unknownBytecode printString
]

{ #category : '*Debugging-Core' }
Context >> restart [
	"Unwind thisContext to self and resume from beginning.  Execute unwind blocks when unwinding.  ASSUMES self is a sender of thisContext"

	| context unwindBlock |
	self isDead ifTrue: [self cannotReturn: nil to: self].
	self privRefresh.
	context := thisContext.
	[	context := context findNextUnwindContextUpTo: self.
		context isNil 
	] whileFalse: [
		context unwindComplete ifNil:[
			context unwindComplete: true.
			unwindBlock := context unwindBlock.
			thisContext terminateTo: context.
			unwindBlock value ]].
	
	thisContext terminateTo: self.
	self jump.
]

{ #category : '*Debugging-Core' }
Context >> restartWithNewReceiver: obj [

	self
		swapReceiver: obj;
		restart
]

{ #category : '*Debugging-Core' }
Context >> returnBooleanPrimitive [

	^ 257
]

{ #category : '*Debugging-Core' }
Context >> returnSelfPrimitive [

	^ 256
]

{ #category : '*Debugging-Core' }
Context class >> runSimulated: aBlock [
	"Simulate the execution of the argument, current. Answer the result it 
	returns."

	^ thisContext sender
		runSimulated: aBlock
		contextAtEachStep: [:ignored |]

	"Context runSimulated: [Pen new defaultNib: 5; go: 100]"
]

{ #category : '*Debugging-Core' }
Context >> runSimulated: aBlock contextAtEachStep: block2 [
	"Simulate the execution of the argument, aBlock, until it ends. aBlock 
	MUST NOT contain an '^'. Evaluate block2 with the current context 
	prior to each instruction executed. Answer the simulated value of aBlock."
	| current returnContext exception |
	
	aBlock hasNonLocalReturn
		ifTrue: [ self error: 'simulation of blocks with ^ can run loose' ].
		
	current := [ aBlock
					on: Exception
					do: [ :ex | SimulationExceptionWrapper signalForException: ex  ] ] asContext.
	
	returnContext := Context
			sender: nil
			receiver: self home receiver
			method: self home compiledCode
			arguments: self home arguments.
	
	current pushArgs: Array new from: returnContext.
	
	[current == returnContext]
		whileFalse: [ 
			block2 value: current.
			current := current step ].
	
	exception := returnContext pop.
	exception class == SimulationExceptionWrapper
		ifTrue: [ ^ exception exception signal ].
	^ exception
]

{ #category : '*Debugging-Core' }
Context >> stepIntoQuickMethod [
	^self compiledCode stepIntoQuickMethods
]

{ #category : '*Debugging-Core' }
Context >> stepIntoQuickMethod: aBoolean [

	self compiledCode stepIntoQuickMethods: aBoolean
]

{ #category : '*Debugging-Core' }
Context >> stepToHome: aContext [
	"Resume self until the home of top context is aContext.  Top context may be a block context."

	| home ctxt here error wrapperContext |
	here := thisContext.
	error := nil.
	
	wrapperContext := aContext insertSender: (Context contextOn: Exception do: [ :ex | 
				            error
					            ifNil: [ 
						            			error := ex.
												ex resumeUnchecked: here jump ]
					            ifNotNil: [ 
										ex pass ] ]).
	home := aContext home.

	"A first Step to do at least one step"
	ctxt := self step.

	home == ctxt home ifTrue: [ 
		"remove above inserted ensure and handler contexts"
		aContext terminateTo: wrapperContext sender.
		^ { 
			  ctxt.
			  error } ].

	[  ctxt := ctxt step.
		error ifNotNil: [ 
		"remove above inserted ensure and handler contexts"
		aContext terminateTo: wrapperContext sender.
			^ { 
				  ctxt.
				  error } ].

		home == ctxt home ] whileFalse: [ 
			(home isDead or: [ ctxt willFailReturn ]) ifTrue: [ 
				^ { 
					  ctxt.
					  nil } ] ].
	"remove above inserted ensure and handler contexts"
	aContext terminateTo: wrapperContext sender.
	
	^ { 
		  ctxt.
		  nil }
]

{ #category : '*Debugging-Core' }
Context >> stepToSendOrReturn [
	"Simulate the execution of bytecodes until either sending a message or 
	returning a value to the receiver (that is, until switching contexts)."

	| stream context |
	stream := InstructionStream on: method pc: pc.	
	[ self isDead or: [ stream willSend or: [ stream willReturn or: [ stream willStore or: [ stream willCreateBlock ] ] ] ] ]
		whileFalse: [
			context := stream interpretNextInstructionFor: self.
			context == self ifFalse: [
				"Caused by mustBeBoolean handling"
				^context ]]
]

{ #category : '*Debugging-Core' }
Context class >> tallyInstructions: aBlock [
	"Count the occurrences of each bytecode during the execution of aBlock.
	Return a Array of associations using the byte as key and the occurrences as values sorted by the instruction opcode numeric values.
	
	This method uses the in-image bytecode interpreter to evaluate and count the instructions."
	
	"(Context tallyInstructions: [3.14159 printString]) size >>> 120"
	
	| tallies |
	tallies := Bag new.
	thisContext sender
		runSimulated: aBlock
		contextAtEachStep:
			[:current | tallies add: current instructionStream peekByte ].
	^tallies sortedElements
]

{ #category : '*Debugging-Core' }
Context class >> tallyMethods: aBlock [
	"This method uses the simulator to count the number of calls on each method
	invoked in evaluating aBlock. Results are given in order of decreasing counts."
	| prev tallies |
	tallies := Bag new.
	prev := aBlock.
	thisContext sender
		runSimulated: aBlock
		contextAtEachStep:
			[:current |
			current == prev ifFalse: "call or return"
				[prev sender ifNotNil: "call only"
					[tallies add: current printString].
				prev := current]].
	^ tallies sortedCounts

	"Contex tallyMethods: [3.14159 printString]"
]

{ #category : '*Debugging-Core' }
Context >> temporaryVariableNamed: aName [
	(self hasTemporaryVariableNamed: aName)
		ifFalse: [ ^ nil ].
	^self lookupVar: aName
]

{ #category : '*Debugging-Core' }
Context >> unusedBytecode [
	^ self respondsToUnknownBytecode
]

{ #category : '*Debugging-Core' }
Context >> updatePCForQuickPrimitiveRestart [

	({self returnBooleanPrimitive.	self returnSelfPrimitive } includes: self method primitive) 
		ifTrue: [ 
			pc := self method endPC.
			^ self ].

	pc := pc + self quickMethodPrimitiveBytecodeSize
]
