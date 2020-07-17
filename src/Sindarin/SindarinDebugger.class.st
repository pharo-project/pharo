"
# Start
Get a ScriptableDebugger instance by doing: `ScriptableDebugger debug: [ <your execution> ]`.
Alternatively, you can get a ScriptableDebugger instance attached on an already existing DebugSession by doing: `ScriptableDebugger attach: aDebugSession` 

# Breakpoints
ScriptableDebugger uses the VirtualBreakpoints class for its breakpoints. 
The breakpoints set by ScriptableDebugger are ""virtual"", in the sense that they do not modify any bytecode (as common breakpoints do) and do not show up in the rest of the IDE. They are simply markers indicating that the scritpable debugger should stop the debugged execution if it reaches an ast node or method on which a virtual breakpoint has been set. A virtual breakpoint set by a scriptable debugger instance is ""visible"" by all other scriptable debugger instances.

Virtual breakpoints were introduced because due to technical limitations, normal breakpoints cannot be set in methods that are already in the stack of the debugged execution.

# Instance Variables:
- process: the (suspended) Process in which the debugged execution takes place
- debugSession: the DebugSession monitoring the debugged execution.
- stepHooks: OrderedCollection<Block>. A list of blocks to be evaluated after each step of the debugged execution
"
Class {
	#name : #SindarinDebugger,
	#superclass : #Object,
	#instVars : [
		'process',
		'stepHooks',
		'nodeMapForMethod',
		'debugStarted',
		'sindarinSession',
		'blockToDebug'
	],
	#category : #Sindarin
}

{ #category : #start }
SindarinDebugger class >> attachTo: aDebugSession [
	"Returns a new instance of ScriptableDebugger attached to aDebugSession"
	^ self new attachTo: aDebugSession.

]

{ #category : #start }
SindarinDebugger class >> debug: aBlock [
	"Creates an execution of aBlock and returns an instance of ScriptableDebugger attached to this execution"
	^ self new debug: aBlock

]

{ #category : #'graphical debugger' }
SindarinDebugger >> activateAutoRefreshOfAttachedGraphicalDebugger [
	sindarinSession activateEventTriggering.
	self refreshAttachedGraphicalDebugger
]

{ #category : #'step hook' }
SindarinDebugger >> addStepHook: aBlock [
	"Adds aBlock to the list of step hooks. Step hooks are executed after each step of the execution"

	stepHooks add: aBlock
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> arguments [
	"Returns the arguments of the current stack-frame."

	^ self context arguments
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> assignmentValue [
	"Returns the value about to be assigned, if the current node is an assignment node. Otherwise, returns nil"

	self node isAssignment
		ifFalse: [ ^ nil	"Error signal: 'Not about to perform a assignment'" ].
	^ self context at: self currentContextStackSize
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> assignmentVariableName [
	"Returns the variable name about to be assigned to, if the current node is an assignment node. Otherwise, returns nil"

	self node isAssignment
		ifFalse: [ ^ nil	"Error signal: 'Not about to perform a assignment'" ].
	^ self node variable name
]

{ #category : #start }
SindarinDebugger >> attachTo: aDebugSession [
	"Attaches this scriptable debugger to an already existing instance of DebugSession or SindarinDebugSession"

	sindarinSession := aDebugSession asSindarinDebugSession.
	process := aDebugSession interruptedProcess.
	sindarinSession deactivateEventTriggering.
	debugStarted := true
]

{ #category : #astAndAstMapping }
SindarinDebugger >> bestNodeFor: anInterval [
	^self node methodNode bestNodeFor: anInterval
]

{ #category : #stackAccess }
SindarinDebugger >> context [
	"Returns a reification of the current stack-frame."

	^ self debugSession interruptedContext
]

{ #category : #private }
SindarinDebugger >> contextIsAboutToSignalException: aContext [
	"Returns whether aContext is about to execute a message-send of selector #signal to an instance of the Exception class (or one of its subclasses)"

	| node |
	node := (self nodeMapForMethod: aContext method) nodeForPC: aContext pc.
	node isMessage
		ifFalse: [ ^ false ].
	node selector = #signal
		ifFalse: [ ^ false ].
	aContext basicSize >= 1
		ifFalse: [ ^ false ].
	(Exception allSubclasses includes: (aContext at: aContext basicSize))
		ifTrue: [ ^ true ].	"#signal sent to a subclass of Exception"
	(Exception allSubclasses
		includes: (aContext at: aContext basicSize) class)
		ifTrue: [ ^ true ].	"#signal sent to an instance of a subclass of Exception"
	^ false
]

{ #category : #'stepping -  auto' }
SindarinDebugger >> continue [
	"Steps the execution until it:
	- is about to signal a Halt exception. In this case return #halt.
	- is about to signal a Break exception. In this case return #breakpoint.
	- has reached a virtual breakpoint. In this case return the virtual breakpoint.
	- has finished. In this case return nil."

	| signalMessageNode exceptionClassBeingSignalled stepHook |
	self flag: 'Sorry, this is shit :)'.
	stepHook := [ VirtualBreakpoint all
		detect: [ :vbrkpt | vbrkpt hitTest: self node ]
		ifFound: [ :vbrkpt | 
			self removeStepHook: stepHook.
			^ vbrkpt ] ].
	self addStepHook: stepHook.
	[ self isExecutionFinished
		or: [ self contextIsAboutToSignalException: self context ] ]
		whileFalse: [ self step ].
	self isExecutionFinished
		ifTrue: [ ^ nil ].
	signalMessageNode := self node.
	exceptionClassBeingSignalled := self context
		at: self context size - signalMessageNode arguments size.
	exceptionClassBeingSignalled == Halt
		ifTrue: [ "Encountered a Halt (typically signalled by a 'self halt')"
			self skip.
			self removeStepHook: stepHook.
			^ #halt ].
	exceptionClassBeingSignalled == Break
		ifTrue: [ "Encountered a Break (i.e. a breakpoint)"
			self skip.
			self removeStepHook: stepHook.
			^ #breakpoint ]
]

{ #category : #acccessing }
SindarinDebugger >> currentBytecode [
	^ self context method symbolicBytecodes
]

{ #category : #private }
SindarinDebugger >> currentContextStackSize [
	^ self context basicSize
]

{ #category : #process }
SindarinDebugger >> currentProcess [
	^process
]

{ #category : #'graphical debugger' }
SindarinDebugger >> deactivateAutoRefreshOfAttachedGraphicalDebugger [
	sindarinSession deactivateEventTriggering
]

{ #category : #start }
SindarinDebugger >> debug: aBlock [
	blockToDebug := aBlock.
	process := aBlock newProcess
		name: 'ExecutionDebuggedByScriptableDebugger'.

	"process on: Exception do: [:ex | DebuggedExecutionSignalledAnException signalWithException: ex. ex resume ]."
	sindarinSession := SindarinDebugSession
		newWithName: 'ScriptableDebuggerDebugSession'
		forProcess: process.
	sindarinSession deactivateEventTriggering.
	[ self selector = #newProcess ] whileFalse: [ self step ].	"Step the process to get out of the on:do: context added at the bottom of its stack"
	[ self selector = #newProcess ] whileTrue: [ self step ].	"Step the process so that it leaves BlockClosure>>#newProcess and enters the block for which a process was created"
	debugStarted := true
]

{ #category : #accessing }
SindarinDebugger >> debugSession [
	"Returns the DebugSession representing the execution this ScriptableDebugger is debugging"

	^ sindarinSession debugSession
]

{ #category : #private }
SindarinDebugger >> hasSignalledUnhandledException [
	"Returns true if the debugged execution has signalled an exception that has not been handled by any on:do: (i.e. the #defaultAction of the exception is about to be executed. This default action typically leads to opening a debugger on the process that signalled the exception)"
	^ (self selector = #defaultAction ) and: [ self receiver isKindOf: Exception ]
]

{ #category : #initialization }
SindarinDebugger >> initialize [
	stepHooks := OrderedCollection new.
	debugStarted := false
]

{ #category : #private }
SindarinDebugger >> isAboutToSignalException [
	"OLD METHOD. SEE #hasSignalledUnhandledException"
	"Returns whether the debugged execution is about to signal an exception"
	^ (self node isMessage
		and: [ self messageSelector = #unhandledErrorDefaultAction:
				and: [ self messageReceiver isKindOf: UIManager ] ])
]

{ #category : #stackAccess }
SindarinDebugger >> isExecutionFinished [
	"Returns whether the debugged execution is finished"

	^ process isTerminating
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> message: aSelector [
	"Returns whether the execution is about to send a message of selector @aSelector to any object"

	| node |
	node := self node.
	node isMessage
		ifFalse: [ ^ false ].
	node selector = aSelector
		ifFalse: [ ^ false ].
	^ true
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> message: aSelector to: anObject [
	"Returns whether the execution is about to send a message of selector @aSelector to @anObject"

	| node |
	node := self node.
	node isMessage
		ifFalse: [ ^ false ].
	node selector = aSelector
		ifFalse: [ ^ false ].
	self messageReceiver == anObject
		ifFalse: [ ^ false ].
	^ true
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> message: aSelector toInstanceOf: aClass [
	"Returns whether the execution is about to send a message of selector @aSelector to an instance of class @aClass"

	| node |
	node := self node.
	node isMessage
		ifFalse: [ ^ false ].
	node selector = aSelector
		ifFalse: [ ^ false ].
	(self messageReceiver isKindOf: aClass)
		ifFalse: [ ^ false ].
	^ true
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> messageArguments [
	"Returns the arguments of the message about to be sent, if the current node is a message node."

	| argumentNumber arguments i |
	self node isMessage
		ifFalse: [ Error signal: 'Not about to send a message' ].
	argumentNumber := self node arguments size.
	arguments := OrderedCollection new.
	i := 0.
	[ i = argumentNumber ]
		whileFalse: [ arguments
				add:
					(self context
						at: self currentContextStackSize - argumentNumber + i + 1).
			i := i + 1 ].
	^ arguments
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> messageReceiver [
	"Returns the receiver of the message about to be sent, if the current node is a message node."

	self node isMessage
		ifFalse: [ Error signal: 'Not about to send a message' ].
	^ self context
		at: self currentContextStackSize - self node arguments size
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> messageSelector [
	"Returns the selector of the message about to be sent, if the current node is a message node."

	self node isMessage
		ifFalse: [ Error signal: 'Not about to send a message' ].
	^ self node selector
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> method [
	"Returns the method of the current stack-frame."

	^ self context method
]

{ #category : #astAndAstMapping }
SindarinDebugger >> node [
	"Returns the AST node about to be executed by the top context of the execution"

	debugStarted
		ifFalse:
			[ "Until the debug session is started, node is returned using the unoptimized version" "Sindarin starts by executing controlled steps to exit the caller code (e.g. tests)
		before entering the actual debugged code, and we do not want to cache nodes for the 
		caller code." ^ self context method sourceNodeForPC: self context pc ].
	"Once the hand is given to the tool or user, we start caching nodes for better performance"
	^ self nodeMap nodeForPC: self context pc
]

{ #category : #astAndAstMapping }
SindarinDebugger >> nodeForContext: aContext [
	"Returns the AST node about to be executed by the top context of the execution"

	^ aContext method sourceNodeForPC: aContext pc
]

{ #category : #astAndAstMapping }
SindarinDebugger >> nodeMap [
	^ self nodeMapForMethod: self context method
]

{ #category : #astAndAstMapping }
SindarinDebugger >> nodeMapForMethod: aCompiledMethod [
	nodeMapForMethod ifNil: [ nodeMapForMethod := Dictionary new ].
	^ (nodeMapForMethod
		at: aCompiledMethod methodClass name
		ifAbsentPut: [ Dictionary new ])
		at: aCompiledMethod selector
		ifAbsentPut: [ SindarinBytecodeToASTCache
				generateForCompiledMethod: aCompiledMethod ]
]

{ #category : #'graphical debugger' }
SindarinDebugger >> openInGraphicalDebugger [
	sindarinSession canBeTerminated: false.	"Prevents the graphical debugger from terminating the debug session when it's closed."
	self
		flag:
			'Should be an extension of DebuggerSelector and handled by its sole instance'
]

{ #category : #accessing }
SindarinDebugger >> pc [
	^ self context pc
]

{ #category : #'stepping -  auto' }
SindarinDebugger >> proceed [
	"alias of #continue"

	^ self continue
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> receiver [
	"Returns the receiver of the current stack-frame."

	^ self context receiver
]

{ #category : #'graphical debugger' }
SindarinDebugger >> refreshAttachedGraphicalDebugger [
	sindarinSession refreshAttachedDebugger
]

{ #category : #'step hook' }
SindarinDebugger >> removeStepHook: aBlock [
	"Remove aBlock from the list of step hooks"

	stepHooks remove: aBlock
]

{ #category : #stackAccess }
SindarinDebugger >> restart [
	"Resets this debugger on a new execution of the block passed as argument to the initial call to #debug:"
	self initialize.
	self debug: blockToDebug.
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> selector [
	"Returns the selector of the current stack-frame."

	^ self context selector
]

{ #category : #breakpoints }
SindarinDebugger >> setBreakpoint [
	"Sets a breakpoint on the current node, returns an object reifying the breakpoint."

	^ self setBreakpointOn: self node
]

{ #category : #breakpoints }
SindarinDebugger >> setBreakpointOn: target [
	"Sets a breakpoint on target (a node or a compiled method), returns an object reifying the breakpoint."

	| astTarget |
	astTarget := target.
	(target isKindOf: CompiledMethod)
		ifTrue: [ astTarget := target ast ].
	^ VirtualBreakpoint newOnNode: astTarget setBy: self
]

{ #category : #private }
SindarinDebugger >> signalExceptionIfDebuggedExecutionHasSignalledUnhandledException [
	| unhandledException |
	(self hasSignalledUnhandledException)
		ifTrue: [ 
			unhandledException := self receiver.
			UnhandledExceptionSignalledByADebuggedExecution
				signalWithException: unhandledException.
		]
	
		"ifTrue:
			[" "The debugged execution signalled an exception, this exception was not handled and is about to cause a debugger to open."
			"Signalling an exception **in the scriptable debugger's process** to inform the user of this"
			"unhandledException := self messageArguments at: 1.
			UnhandledExceptionSignalledByADebuggedExecution
				signalWithException: unhandledException ]"
]

{ #category : #private }
SindarinDebugger >> signalExceptionIfDebuggedExecutionIsFinished [
	"Signals an DebuggedExecutionIsFinished exception if the debugged execution is finished"
	(self isExecutionFinished) ifTrue: [ 
		DebuggedExecutionIsFinished signal.
	].
]

{ #category : #'stepping -  skip' }
SindarinDebugger >> skip [
	"If it is a message send or assignment, skips the execution of the current instruction, and puts nil on the execution stack."

	self skipWith: nil
]

{ #category : #'stepping -  skip' }
SindarinDebugger >> skipAssignmentNodeWith: replacementValue [
	self context pop.
	"Pop the value to be assigned"
	"Push the replacement value on the context's value stack, to simulate that the assignment happened and had value nil"
	self context push: replacementValue.
	"Increase the pc to go over the assignment"
	self context pc: self context pc + 2.
	"Execute bytecodes the debugger usually executes without stopping the execution (for example popping the return value of the just executed message send if it is not used afterwards)"
	self debugSession
		stepToFirstInterestingBytecodeIn: self debugSession interruptedProcess
]

{ #category : #'stepping -  skip' }
SindarinDebugger >> skipMessageNodeWith: replacementValue [
	self node arguments do: [ :arg | self context pop ].	"Pop the arguments of the message send from the context's value stack"
	"Pop the receiver from the context's value stack"
	self context pop.
	"Push the replacement value on the context's value stack, to simulate that the message send happened and returned nil"
	self context push: replacementValue.
	"Increase the pc to go over the message send"
	self context pc: self context pc + 1.
	"Execute bytecodes the debugger usually executes without stopping the execution (for example popping the return value of the just executed message send if it is not used afterwards)"
	self debugSession
		stepToFirstInterestingBytecodeIn: self debugSession interruptedProcess
]

{ #category : #'stepping -  skip' }
SindarinDebugger >> skipThroughNode: aProgramNode [
	"Skips execution until program counter reaches aProgramNode.
	Also skip the target node."

	self skipUpToNode: aProgramNode skipTargetNode: true
]

{ #category : #'stepping -  skip' }
SindarinDebugger >> skipToPC: aPC [
	"Skips execution until program counter reaches aPC."
	[ self pc >= aPC ] whileFalse: [ self skip ]
]

{ #category : #'stepping -  skip' }
SindarinDebugger >> skipUpToNode: aProgramNode [
	"Skips execution until program counter reaches aProgramNode.
	Does not skip the target node."

	self skipUpToNode: aProgramNode skipTargetNode: false
]

{ #category : #'stepping -  skip' }
SindarinDebugger >> skipUpToNode: aProgramNode skipTargetNode: skipTargetNode [
	"Skips execution until program counter reaches aProgramNode."

	[ self node == aProgramNode ] whileFalse: [ self skip ].
	aProgramNode isReturn	ifTrue: [ ^ self ].
	skipTargetNode ifTrue: [ self skip ]
]

{ #category : #'stepping -  skip' }
SindarinDebugger >> skipWith: replacementValue [
	"If it is a message-send or assignment, skips the execution of the current instruction, and puts the replacementValue on the execution stack."

	"If the current node is a message send or assignment"

	(self node isMessage not
		and: [ self node isAssignment not ])
		ifTrue: [ ^ self ].
	self node isMessage
		ifTrue: [ ^ self skipMessageNodeWith: replacementValue ].
	self node isAssignment
		ifTrue: [ ^	self skipAssignmentNodeWith: replacementValue ]
]

{ #category : #stackAccess }
SindarinDebugger >> stack [
	"Returns a list of context objects representing the current call stack."

	^ self debugSession stack
]

{ #category : #'stepping - steps' }
SindarinDebugger >> step [
	"Executes the next instruction. If the instruction is a message-send, step inside it."

	self signalExceptionIfDebuggedExecutionHasSignalledUnhandledException.
	self signalExceptionIfDebuggedExecutionIsFinished.
	self debugSession stepInto.
	stepHooks do: [ :aBlock | aBlock value ]
]

{ #category : #'stepping - steps' }
SindarinDebugger >> step: anInt [
	"Call the #step method @anInt times"

	anInt timesRepeat: [ self step ]
]

{ #category : #'as yet unclassified' }
SindarinDebugger >> stepBatchAndReturnNodeHashAndStackSize [
	"For EchoDebugger"
	| nodesHashAndStackSize count |
	count := 0.
	nodesHashAndStackSize := OrderedCollection new.
	[ [ count <= 1000] whileTrue: [ count := count + 1. nodesHashAndStackSize add: {self node dictionaryRepresentation hash. self stack size}. self step ]] on: DebuggedExecutionException do: [ "when debugged execution is over or signals exception, stop and return the result data" ^ nodesHashAndStackSize ].
	^ nodesHashAndStackSize
	
]

{ #category : #'stepping - steps' }
SindarinDebugger >> stepBytecode [
	"Executes the next bytecode"

	self flag: 'Needs to be tested'.
	self signalExceptionIfDebuggedExecutionHasSignalledUnhandledException.
	process completeStep: self debugSession context.
	self debugSession updateContextTo: process suspendedContext
]

{ #category : #'stepping - steps' }
SindarinDebugger >> stepOver [
	| startContext |
	startContext := self context.
	self step.
	[ self context == startContext
		or: [ startContext sender isNil
				or: [ startContext hasSender: self context ] ] ]
		whileFalse: [ self step ]
]

{ #category : #'stepping - steps' }
SindarinDebugger >> stepOver: anInt [
	"Call the #stepOver method @anInt times"

	anInt timesRepeat: [ self stepOver ]
]

{ #category : #'stepping - steps' }
SindarinDebugger >> stepThrough [
	"Hacked for demonstration purposes to have a stepThrough"

	self signalExceptionIfDebuggedExecutionHasSignalledUnhandledException.
	self debugSession stepThrough.
	stepHooks do: [ :aBlock | aBlock value ]
]

{ #category : #'stepping - steps' }
SindarinDebugger >> stepToMethodEntry [
	self stepUntil: [ self context willSend ].
	process step: self context.
	self debugSession updateContextTo: process suspendedContext
]

{ #category : #'stepping - steps' }
SindarinDebugger >> stepToReturn [
	[ self context willReturn ] whileFalse: [self debugSession stepThrough]
]

{ #category : #'stepping - steps' }
SindarinDebugger >> stepUntil: aBlock [
	"Steps the execution until aBlock evaluates to true"

	aBlock whileFalse: [ self step ]
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> temporaries [
	"Returns the temporary variables of the current stack-frame."

	^ self context temporaries
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> temporaryNamed: aSymbol [
	^self temporaries at: aSymbol
]

{ #category : #stackAccessHelpers }
SindarinDebugger >> topStack [
	^self context top
]
