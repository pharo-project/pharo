Extension { #name : #Context }

{ #category : #'*Debugging-Core' }
Context >> callChainAnySatisfy: aBlock [

	(aBlock value: self) ifTrue: [ ^ true ].
	self sender ifNil: [ ^ false ].
	^ self sender callChainAnySatisfy: aBlock.
]

{ #category : #'*Debugging-Core' }
Context >> callPrimitive: primNumber [
	| res |
	res := receiver tryPrimitive: primNumber withArgs: self arguments.
	primNumber >= (1 << 15) ifTrue: [ "inlined primitive, cannot fail" ^ self push: res ].  
	"regular primitive. Always at the beginning of methods."
	(self isFailToken: res) ifTrue: [ "keeps interpreting the method" ^ self ].
	self push: res.
	self methodReturnTop
]

{ #category : #'*Debugging-Core' }
Context >> completeCallee: aContext [
	"Simulate the execution of bytecodes until a return to the receiver."
	| context current nextContext |
	context := aContext.
	[ context == current or: [ context hasSender: self ] ]
		whileTrue: [
			current := context.
			nextContext := context quickStep.
			nextContext ifNil: [ self halt ].
			context := nextContext ].
	^ self stepToSendOrReturn
]

{ #category : #'*Debugging-Core' }
Context >> lookupSymbol: aSymbol [
	| scope var |
	scope := self sourceNodeExecuted scope.
	var := scope lookupVar: aSymbol asString.
	"Local variables"
	(var isKindOf: OCTempVariable) ifTrue: [^ var searchFromContext: self scope: scope].
	"Instance variables"
	(var isKindOf: OCSlotVariable) ifTrue: [^ self receiver instVarNamed: aSymbol].
	"Class variables and globals"
	(var isKindOf: OCLiteralVariable) ifTrue: [ ^ var assoc value ].
	^ nil.
	
]

{ #category : #'*Debugging-Core' }
Context >> methodReturnConstant: value [
	"Simulate the action of a 'return constant' bytecode whose value is the
	 argument, value. This corresponds to a source expression like '^0'."

	^self return: value from: self methodReturnContext
]

{ #category : #'*Debugging-Core' }
Context >> namedTempAt: index [
	"Answer the value of the temp at index in the receiver's sequence of tempNames."
	^self debuggerMap namedTempAt: index in: self
]

{ #category : #'*Debugging-Core' }
Context >> namedTempAt: index put: aValue [
	"Set the value of the temp at index in the receiver's sequence of tempNames.
	 (Note that if the value is a copied value it is also set out along the lexical chain,
	  but alas not in along the lexical chain.)."
	^self debuggerMap namedTempAt: index put: aValue in: self
]

{ #category : #'*Debugging-Core' }
Context >> previousPcWithCorrectMapping [
	"Answer a pc inside the enclosing block or mathod that is correctly mapped to an AST node"
	"This is an ugly and temporary fix for Pharo 3. 
	Must be removed as soon as mappings are fixed"
	| neighborPC |
	neighborPC := self isDead 
		ifTrue: [ self startpc ]
		ifFalse: [ self previousPc ifNil: [self startpc]].
	"There is a pushLiteral: nil bytecode for each temps in a block. 
	There is a 'pushTemp:' bytecode for each copied value of a block.
	These bytecodes are not mapped to any IR.
	We skip both"
	[ self isPushLiteralNil: neighborPC ] whileTrue: [ neighborPC := neighborPC + 1 ].
	[ self isPushTemp: neighborPC ] whileTrue: [ neighborPC := neighborPC + 1 ].
	"The block return ir instruction is mapped to the block instead of the sequence AST node. So we go just before it"
	(self isBlockReturn: neighborPC) ifTrue: [ neighborPC := neighborPC - 1 ].
	^ neighborPC
]

{ #category : #'*Debugging-Core' }
Context >> quickSend: selector to: aReceiver with: arguments super: superFlag [
	"Send the given selector with arguments in an environment which closely resembles the non-simulating environment, with an interjected unwind-protected block to catch nonlocal returns.
	Attention: don't get lost!"
	| lookupClass contextToReturnTo result |
	contextToReturnTo := self.
	lookupClass := superFlag
					ifTrue: [(self method literalAt: self method numLiterals) value superclass]
					ifFalse: [self objectClass: aReceiver].
	[ | oldSender |
	oldSender := thisContext sender swapSender: self.
	result := self object: aReceiver perform: selector withArguments: arguments inClass: lookupClass.
	thisContext sender swapSender: oldSender] ifCurtailed: [
		contextToReturnTo := thisContext sender receiver.	"The block context returning nonlocally"
		contextToReturnTo jump: -1.	"skip to front of return bytecode causing this unwind"
		contextToReturnTo nextByte = 16r7C ifTrue: [
			"If it was a returnTop, push the value to be returned.
			Otherwise the value is implicit in the bytecode"
			contextToReturnTo push: (thisContext sender tempAt: 1)].
		thisContext swapSender: thisContext home sender.	"Make this block return to the method's sender"
		contextToReturnTo].
	contextToReturnTo push: result.
	^contextToReturnTo
]

{ #category : #'*Debugging-Core' }
Context >> quickStep [
	"If the next instruction is a send, just perform it.
	Otherwise, do a normal step."

	self willSend ifTrue: [ QuickStep := self ].
	^self step
]

{ #category : #'*Debugging-Core' }
Context >> respondsToUnknownBytecode [
	"This method is triggerred by the VM when the interpreter tries to execute an unknown bytecode"
	
	| unknownBytecode |
	unknownBytecode := self method at: self pc.
	self error: 'VM cannot run unknown bytecode ', unknownBytecode printString
	

]

{ #category : #'*Debugging-Core' }
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

{ #category : #'*Debugging-Core' }
Context >> restartWithNewReceiver: obj [

	self
		swapReceiver: obj;
		restart
]

{ #category : #'*Debugging-Core' }
Context >> return: value to: aSender [
	"Simulate the return of value to aSender."

	self releaseTo: aSender.
	aSender ifNil: [^ nil].
	^ aSender push: value
]

{ #category : #'*Debugging-Core' }
Context class >> runSimulated: aBlock [
	"Simulate the execution of the argument, current. Answer the result it 
	returns."

	^ thisContext sender
		runSimulated: aBlock
		contextAtEachStep: [:ignored |]

	"Context runSimulated: [Pen new defaultNib: 5; go: 100]"
]

{ #category : #'*Debugging-Core' }
Context >> runSimulated: aBlock contextAtEachStep: block2 [
	"Simulate the execution of the argument, aBlock, until it ends. aBlock 
	MUST NOT contain an '^'. Evaluate block2 with the current context 
	prior to each instruction executed. Answer the simulated value of aBlock."
	| current returnContext exception |
	
	aBlock hasMethodReturn
		ifTrue: [ self error: 'simulation of blocks with ^ can run loose' ].
		
	current := [ aBlock
					on: Exception
					do: [ :ex | SimulationExceptionWrapper signalForException: ex  ] ] asContext.
	
	returnContext := Context
			sender: nil
			receiver: self home receiver
			method: self home method
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

{ #category : #'*Debugging-Core' }
Context >> send: selector super: superFlag numArgs: numArgs [
	"Simulate the action of bytecodes that send a message with selector, 
	selector. The argument, superFlag, tells whether the receiver of the 
	message was specified with 'super' in the source method. The arguments 
	of the message are found in the top numArgs locations on the stack and 
	the receiver just below them."

	| currentReceiver arguments  |
	arguments := Array new: numArgs.
	numArgs to: 1 by: -1 do: [ :i | 
		arguments at: i put: self pop ].
	currentReceiver := self pop.
"	selector == #doPrimitive:method:receiver:args:
		ifTrue: [answer := receiver 
					doPrimitive: (arguments at: 1)
					method: (arguments at: 2)
					receiver: (arguments at: 3)
					args: (arguments at: 4).
				self push: answer.
				^self]. "
	QuickStep == self ifTrue: [
		QuickStep := nil.
		^ self quickSend: selector to: currentReceiver with: arguments super: superFlag].
	^ self send: selector to: currentReceiver with: arguments super: superFlag
]

{ #category : #'*Debugging-Core' }
Context >> simulatePrimitive: primitiveIndex in: aMethod receiver: aReceiver arguments: arguments [
	| key simulator |
	
	key := primitiveIndex = 117 
		ifTrue: [ | literal |
			literal := aMethod literalAt: 1.
			"primitive name, module name"
			{literal second. literal first}] 
		ifFalse: [ primitiveIndex ].
	
	simulator := self class specialPrimitiveSimulators at: key ifAbsent: [
		 "named primitives"
		^  primitiveIndex = 117
				ifTrue: [
					self withoutPrimitiveTryNamedPrimitiveIn: aMethod for: aReceiver withArgs: arguments.
					"this uses primitive 218, which doesn't works as expected... 
					self tryNamedPrimitiveIn: method for: receiver withArgs: arguments " ]
				ifFalse: [ aReceiver tryPrimitive: primitiveIndex withArgs: arguments ]]. 
	
	^ simulator 
		simulatePrimitiveFor: aMethod
		receiver: aReceiver
		arguments: arguments
		context: self
]

{ #category : #'*Debugging-Core' }
Context class >> simulatePrimitive: primName module: moduleName with: simulator [
	^ self specialPrimitiveSimulators at: {primName. moduleName} put: simulator
]

{ #category : #'*Debugging-Core' }
Context class >> simulatePrimitiveNumber: num with: simulator [
	^ self specialPrimitiveSimulators at: num put: simulator
]

{ #category : #'*Debugging-Core' }
Context >> stepToHome: aContext [ 
	"Resume self until the home of top context is aContext.  Top context may be a block context."

	| home ctxt here error topContext context |
	
	here := thisContext.
	ctxt := self step.
	ctxt = self ifFalse: [ 
		"Insert ensure and exception handler contexts under aSender"
		error := nil.
		context := aContext insertSender: (Context
			contextOn: UnhandledError do: [:ex |
				error ifNil: [
					"this is ugly but it fixes the side-effects of not sending an Unhandled error on Halt"
					error := (ex isKindOf: Halt) ifTrue: [ ex ] ifFalse: [ ex exception ].
					topContext := thisContext.
					ex resumeUnchecked: here jump ]
						ifNotNil: [ ex pass ]]).
	].
	home := aContext home.
	home == ctxt home ifTrue: [ ^{ctxt. nil} ].
	
	[ctxt := ctxt step.
	error ifNotNil: [
		"Error was raised, remove inserted above contexts then return signaler context"
			"aContext terminateTo: context sender."  "remove above ensure and handler contexts"
			^ {ctxt. error}].
	
	home == ctxt home] whileFalse: [
		home isDead ifTrue: [^ {ctxt. nil}]
	].

	^ {ctxt. nil}
]

{ #category : #'*Debugging-Core' }
Context >> stepToSendOrReturn [
	"Simulate the execution of bytecodes until either sending a message or 
	returning a value to the receiver (that is, until switching contexts)."

	| context |
	[ self willSend or: [  self willReturn or: [ self willStore or: [self willCreateBlock ] ] ] ]
		whileFalse: [
			self isDead ifTrue: [ ^ self ].
			context := self step.
			context == self ifFalse: [
				"Caused by mustBeBoolean handling"
				^context ]]
]

{ #category : #'*Debugging-Core' }
Context class >> tallyInstructions: aBlock [
	"This method uses the simulator to count the number of occurrences of
	each of the Smalltalk instructions executed during evaluation of aBlock.
	Results appear in order of the byteCode set."
	| tallies |
	tallies := Bag new.
	thisContext sender
		runSimulated: aBlock
		contextAtEachStep:
			[:current | tallies add: current nextByte].
	^tallies sortedElements

	"Context tallyInstructions: [3.14159 printString]"
]

{ #category : #'*Debugging-Core' }
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

{ #category : #'*Debugging-Core' }
Context >> tempNamed: aName [
	"Returns the value of the temporaries, aName."

	"Implementation notes: temporary initialization in blocks simply uses pushNil to allocate and initialize each temp.  So if one inspects [|a|a:=2] and sends it self method symbolic you get:

	13 <8F 00 00 05> closureNumCopied: 0 numArgs: 0 bytes 17 to 21
	17 	<73> pushConstant: nil
	18 	<77> pushConstant: 2
	19 	<81 40> storeIntoTemp: 0
	21 	<7D> blockReturn
	22 <7C> returnTop

	And when we check self asContext pc we get 17, which is *before* the nil is pushed. Therefore we should pay attention when querying a temporary if the temporary allocation was executed."

	| index |
	index := self tempNames indexOf: aName.
	^ self namedTempAt: index
]

{ #category : #'*Debugging-Core' }
Context >> tempNamed: aName put: anObject [
	^self namedTempAt: (self tempNames indexOf: aName) put: anObject
]

{ #category : #'*Debugging-Core' }
Context >> tempNames [
	"Answer a SequenceableCollection of the names of the receiver's temporary 
	 variables, which are strings."

	^ self debuggerMap tempNamesForContext: self
]

{ #category : #'*Debugging-Core' }
Context >> temporaryVariableNamed: aName [
	(self hasTemporaryVariableNamed: aName)
		ifFalse: [ ^ nil ].
	^TemporaryVariable name: aName context: self

]

{ #category : #'*Debugging-Core' }
Context >> temporaryVariables [
	^self tempNames collect: [ :name | TemporaryVariable new name: name ]

]

{ #category : #'*Debugging-Core' }
Context >> tempsAndValues [
	"Return a string of the temporary variabls and their current values"

	^ String
		streamContents: [ :aStream | 
			self tempNames
				doWithIndex: [ :title :index | 
					aStream
						nextPutAll: title;
						nextPut: $:;
						space;
						tab.
					self print: (self namedTempAt: index) on: aStream.
					aStream newLine ] ]
]

{ #category : #'*Debugging-Core' }
Context >> tempsAndValuesLimitedTo: sizeLimit indent: indent [
	"Return a string of the temporary variabls and their current values"

	^ String
		streamContents: [ :aStream | 
			self tempNames
				doWithIndex: [ :title :index | 
					indent timesRepeat: [ aStream tab ].
					aStream
						nextPutAll: title;
						nextPut: $:;
						space;
						tab.
					aStream nextPutAll: ((self namedTempAt: index) printStringLimitedTo: (sizeLimit - 3 - title size max: 1)).
					aStream newLine ] ]
]

{ #category : #'*Debugging-Core' }
Context >> unusedBytecode [
	^ self respondsToUnknownBytecode 
]
