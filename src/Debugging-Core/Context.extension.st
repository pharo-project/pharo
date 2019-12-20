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
Context >> lookupSymbol: aSymbol [
	| var |
	var := (self sourceNodeExecuted scope) lookupVar: aSymbol.
	"Local variables"
	var isTemp ifTrue: [^ self tempNamed: aSymbol].
	"Instance variables"
	var isInstance ifTrue: [^ self receiver instVarNamed: aSymbol].
	"Class variables and globals"
	var isGlobal ifTrue: [ ^ var assoc value ].
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
	"NOTE: this list of temp names is completely virtual! It omits temp vectors but 
	adds every variable that could be acccessed from a source perspective but might not be"
	^self tempNamed: (self tempNames at: index)
]

{ #category : #'*Debugging-Core' }
Context >> namedTempAt: index put: aValue [
	"Set the value of the temp at index in the receiver's sequence of tempNames"
	"NOTE: this list of temp names is completely virtual! It omits temp vectors but 
	adds every variable that could be acccessed from a source perspective but might not be"
	^self tempNamed: (self tempNames at: index) put: aValue
]

{ #category : #'*Debugging-Core' }
Context >> pcRange [
	"return the debug highlight for aPC"
	^self sourceNodeExecuted debugHighlightRange 
]

{ #category : #'*Debugging-Core' }
Context >> pcRangeContextIsActive: contextIsActive [
 	"return the debug highlight for aPC"		 
	| thePC |

  	"make sure we have some usable value (can happen for contexts in the ProcessBrowser"		
 	thePC := pc ifNil: [self method endPC].		

  	"When on the top of the stack the pc is pointing to right instruction, but deeper in the stack		
 	the pc was already advanced one bytecode, so we need to go back this one bytecode, which		
 	can consist of multiple bytes. But on IR, we record the *last* bytecode offset as the offset of		
 	the IR instruction, which means we can just go back one"		
 	thePC := contextIsActive ifTrue: [thePC] ifFalse: [thePC - 1].		

  	^self method rangeForPC: thePC
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
					error := ex exception.
					topContext := thisContext.
					ex resumeUnchecked: here jump ]
						ifNotNil: [ ex pass ]]).
	].
	home := aContext home.
	home == ctxt home ifTrue: [ ^{ctxt. nil} ].
	
	[ctxt := ctxt step.
	error ifNotNil: [
		"remove above inserted ensure and handler contexts"
		context ifNotNil: [aContext terminateTo: context sender].
		^ {ctxt. error}].
	
	home == ctxt home] whileFalse: [
		home isDead ifTrue: [^ {ctxt. nil}]
	].
	"remove above inserted ensure and handler contexts"
	context ifNotNil: [aContext terminateTo: context sender].
	^ {ctxt. nil}
]

{ #category : #'*Debugging-Core' }
Context >> stepToSendOrReturn [
	"Simulate the execution of bytecodes until either sending a message or 
	returning a value to the receiver (that is, until switching contexts)."

	| context |
	[ self isDead or: [ self willSend or: [  self willReturn or: [ self willStore or: [self willCreateBlock ] ] ] ] ]
		whileFalse: [
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
	"Returns the value of the temporaries, aName"

	| scope var |
	scope := self sourceNodeExecuted scope.
	var := scope lookupVar: aName.
	^var readFromContext: self scope: scope.
]

{ #category : #'*Debugging-Core' }
Context >> tempNamed: aName put: anObject [
	"Assign the value of the temp with name in aContext"
	
	| scope var |
	scope := self sourceNodeExecuted scope.
	var := scope lookupVar: aName.
	^var writeFromContext: self scope: scope value: anObject
]

{ #category : #'*Debugging-Core' }
Context >> tempNames [
	"Answer all the temp names in scope in aContext starting with the home's first local 
	(the first argument or first temporary if no arguments).
	
	These are all the temps that a programmer could access in the context, but keep in mind
	that as they might not be accesses here. 
	In addition, even vars that are accessed in this context could be stored
	in a temp vector, which itself would be a copied temp that has no name..."
	
	^ self sourceNodeExecuted scope allTempNames
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
	
	^ String streamContents: [ :aStream |
		self tempNames 
			do: [ :name  |
			 aStream nextPutAll: name; nextPut: $:; space; tab.
			 self print: (self tempNamed: name) on: aStream]
			separatedBy: [aStream cr ]]
]

{ #category : #'*Debugging-Core' }
Context >> tempsAndValuesLimitedTo: sizeLimit indent: indent [
	"Return a string of the temporary variabls and their current values"

	^ String streamContents: [ :aStream |
		self tempNames 
			do: [ :name |
				indent timesRepeat: [ aStream tab ].
				aStream nextPutAll: name; nextPut: $:; space; tab.
				aStream nextPutAll: 
					((self tempNamed: name) printStringLimitedTo: (sizeLimit -3 -name size max: 1))]
			separatedBy: [aStream cr ]]
]

{ #category : #'*Debugging-Core' }
Context >> unusedBytecode [
	^ self respondsToUnknownBytecode 
]
