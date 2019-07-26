"
Callbacks encapsulate callbacks from the outside world.  They allow Smalltalk blocks to be evaluated and answer their results to external (e.g. C) callees.  Callbacks are created with signature:block:, e.g.

	cb := Callback
			signature:  #(int (*)(const void *, const void *))
			block: [ :arg1 :arg2 | ((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign].

and passed through the FFI by passing their pointer, e.g.

	self qui: data ck: data size so: 8 rt: cb pointer

When the callback is made, the system arranges that the block is invoked with the arguments as defined by the signature, and the result of the block passed back, again as defined by the signature.  See methods in the signatures protocol in subclasses of Callback for signature methods that decode the C stack and registers to invoke a callback with parsed arguments.  See Callback>>valueInContext: and subclass implementations for the evaluation of the signature method that invokes the callback block with correctly parsed arguments.  See Alien class>>invokeCallbackContext: for the entry-point for callbacks into the system from the VM.

Instance Variables:
block <BlockClosure> - The Smalltalk code to be run in response to external code invoking the callback.
thunk <FFICallbackThunk> - the wrapper around the machine-code thunk that initiates the callback and whose address should be passed to C
evaluator <Symbol> - the selector of the marshalling method to use; see methods in the signatures protocol in subclasses of Callback.
numEvaluatorArgs <Integer> - the arity of evaluator
argsProxyClass <Alien subclass> - legacy; unused; the wrapper around the thunk's incoming stack pointer, used to extract arguments from the stack.

Class Variables:
ThunkToCallbackMap <Dictionary of: thunkAddress <Integer> -> callback <Callback>> - used to lookup the Callback associated with a specific thunk address on callback.  See FFICallbackThunk.
ABI <String> - the name of the current ABI

Class Instance Variables
concreteClass <Callback subclass> - the concrete class for callbacks on the current platform, or nil if one doesn't yet exist.

Implementation:
The way that it works is in two parts
- on callback the VM passes up a pointer to a structure from which all arguments, stacked and in registers (because the VM has copied any register args into the struct) can be accessed, and through which the result can be returned.
- the image level provides marshalling methods that match the signature in the callback.  Marshalling methods belong in concrete subclasses, one subclass for each ABI.

So e.g. with a callback of
		Callback
			signature:  #(int (*)(const void *, const void *))
			block: [ :arg1 :arg2 | ((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign]
the marshalling methods are in one of Callback's concrete subclasses signatures protocol, for example

CallbackForIA32>>voidstarvoidstarRetint: callbackContext sp: spAlien
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (spAlien unsignedLongAt: 1))
			value: (Alien forPointer: (spAlien unsignedLongAt: 5)))

where spAlien is an Alien pointing to a VMCallbackContext32.

For ARM support, where there the first four integer arguments are passed in registers, we can use

CallbackForARM32>>voidstarvoidstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (regsAlien unsignedLongAt: 1))
			value: (Alien forPointer: (regsAlien unsignedLongAt: 5)))

The selector of the method doesn't matter, providing it doesn't conflict with any other, except for the number of arguments.  What's important is the pragma which defines the signature and the ABI for which this is a valid marshalling method.  Support for callee pop callbacks (Pascal calling convention such as the Win32 stdcall: convention) are supported using the <calleepops: N> pragma which specifies how many bytes to pop.

When a callback is instantiated, Callback introspects to find the marshalling method that matches the signature for the current ABI.  If one doesn't already exist you can write one.  Hopefully we'll write an ABI compiler that will automatically generate these marshalling methods according to the platform's ABI, but for now its a manual process.; at least it's open and flexible.  When the callback is invoked the evaluator is performed with the current callbackContext and pointer(s) to the arguments.  There is a 32-bit and a 64-bit callback context, and it can have a stack pointer, integer register args and floating point register args, so it's general enough for any callback.

To pass back the result, a value is assigned into the struct via the accessor in the marshalling method and control returns to teh point where teh callback comes in, and this uses a primitive to return.  Inside the callbackContext is a jmpbuf from a setjmp.  The primitive longjmp's back to the entry point in the VM which extracts the result and the code for the kind of result and returns.  See Callback class>>invokeCallbackContext:
"
Class {
	#name : #Callback,
	#superclass : #Object,
	#instVars : [
		'block',
		'thunk',
		'evaluator',
		'numEvaluatorArgs',
		'argsProxyClass'
	],
	#classVars : [
		'ABI',
		'FlushThunkToCallbackMap',
		'ThunkToCallbackMap'
	],
	#classInstVars : [
		'concreteClass'
	],
	#category : #'Alien-Core'
}

{ #category : #accessing }
Callback class >> abi [
	^ABI
]

{ #category : #'instance creation-obsolete' }
Callback class >> block: aBlock "<BlockContext>" argsClass: argsProxyClass [ "<Alien class>" "^<Callback>"
	"Answer a Callback that evaluates aBlock and whose thunk should conform to the C calling convention."
	<legacy>
	^self new
		block: aBlock
		thunk: FFICallbackThunk newCCall
		argsProxyClass: argsProxyClass
]

{ #category : #'instance creation-obsolete' }
Callback class >> block: aBlock "<BlockContext>" stdcallArgsClass: argsProxyClass [ "<Alien class>" "^<Callback>"
	"Answer a Callback that evaluates aBlock and whose thunk should conform to the
	 stdcall (Pascal) calling convention with a stack size of argsProxyClass dataSize."
	<legacy>
	^self new
		block: aBlock
		thunk: (FFICallbackThunk newStdCall: argsProxyClass dataSize)
		argsProxyClass: argsProxyClass
]

{ #category : #'class initialization' }
Callback class >> ensureNotifiedAtStartup [
	"Callback needs to be notified at system startup to clear the ThunkToCallbackMap and determine concreteClass."
	self ~~ Callback ifTrue: [^self].
	(Smalltalk classNamed: #SessionManager)
		ifNotNil: [:sessionManagerClass|
			sessionManagerClass default
				registerSystemClassNamed: self name
				atPriority: 60]
		ifNil: [Smalltalk addToStartUpList: self after: FFICallbackThunk]
]

{ #category : #evaluation }
Callback class >> evaluateCallbackForContext: callbackContext [

	"<VMCallbackContext32|VMCallbackContext64> ^<FFICallbackReturnValue>"

	( ThunkToCallbackMap
		at: callbackContext thunkp
		ifAbsent: [ ^ self error: 'could not locate Callback instance corresponding to thunk address' ] )
		ifNil: [ self error: 'Callback instance for this thunk address has been garbage collected' ]
		ifNotNil: [ :callback | ^ callback valueInContext: callbackContext ]
]

{ #category : #evaluation }
Callback class >> evaluateCallbackForThunk: thunkPtr stack: stackPtr [

	"<Integer>"
	"^<FFICallbackReturnValue>"

	<legacy>
	| callback |

	callback := ThunkToCallbackMap
		at: thunkPtr
		ifAbsent: [ ^ self error: 'could not locate Callback instance corresponding to thunk address' ].
	callback
		ifNil: [ self error: 'Callback instance for this thunk address has been garbage collected' ]
		ifNotNil: [ ^ callback valueWithStackPointer: stackPtr ]
]

{ #category : #finalization }
Callback class >> finalizeValues [
	FlushThunkToCallbackMap := true
]

{ #category : #'class initialization' }
Callback class >> initialize [
	"Callback initialize"
	self ensureNotifiedAtStartup.
	[self startUp: false] "Monticello... grrr...."
		on: Error
		do: [:ex| ].
	(Smalltalk specialObjectsArray at: 54) ~~ #invokeCallbackContext: ifTrue:
		[UIManager default inform: 'this version of Callbacks requires the callback selector to be\#invokeCallbackContext:.  recreate the specialObjects Array.' withCRs]
]

{ #category : #'instance creation' }
Callback class >> newForPlatform [
	"Answer an instance of the subclass for this platform's ABI, if any."
	^concreteClass
		ifNotNil: [:cc| cc new]
		ifNil: [self error: 'could not find a concrete Callback class for this platform']
]

{ #category : #'instance creation' }
Callback class >> signature: signature "<String>" block: aBlock [ "<BlockClosure> ^<Callback>"
	"Answer a Callback that evaluates aBlock whose C signature is signature 
	 and whose thunk should conform to the C calling convention."
	^self newForPlatform signature: signature block: aBlock
]

{ #category : #'class initialization' }
Callback class >> startUp: isInANewSession [
	"Callback startUp: false"
	| prevMap |
	concreteClass := subclasses detect: [:subclass| subclass isForCurrentPlatform] ifNone: [].
	ABI := concreteClass ifNotNil: [concreteClass abi] ifNil: ['UNKNOWN'].
	FlushThunkToCallbackMap := false.
	(WeakArray hasWeakDependent: self) ifFalse:
		[WeakArray addWeakDependent: self].
	prevMap := isInANewSession ifFalse: [ThunkToCallbackMap].
	ThunkToCallbackMap := WeakValueDictionary new.
	prevMap ifNotNil:
		[prevMap keysAndValuesDo:
			[:thunkAddress :callback|
			ThunkToCallbackMap at: thunkAddress put: callback]]
]

{ #category : #private }
Callback >> addToThunkTable [
	FlushThunkToCallbackMap ifTrue:
		[FlushThunkToCallbackMap := false.
		 ThunkToCallbackMap keys do:
			[:k| (ThunkToCallbackMap at: k) ifNil: [ThunkToCallbackMap removeKey: k]]].
	ThunkToCallbackMap at: thunk addressField put: self
]

{ #category : #evaluation }
Callback >> block: aBlock "<BlockClosure>" thunk: thunkWrapper "<FFICallbackThunk>" argsProxyClass: argsWrapperClass [ "<Alien class>"
	<legacy>
	block := aBlock.
	thunk := thunkWrapper.
	argsProxyClass := argsWrapperClass.
	numEvaluatorArgs := 1.
	evaluator := #valueWithContext:.
	self addToThunkTable
]

{ #category : #private }
Callback >> evaluatorForSignature: signature [ "<String|Array>"
	"Search the methods marked with the signature: primtiive for those that match signature.
	 signature is typically a literal Array for the function's C signature, e.g. #(int (*)(int, char *))."
	Pragma withPragmasIn: self class do:
		[:pragma|
		 (pragma key == #signature:
		  and: [(pragma argumentAt: 1) = signature]) ifTrue:
			[^pragma method]].
	self error: 'could not find Callback signature'
]

{ #category : #signatures }
Callback >> intcharstarRetint: callbackContext regs: regsAlien [
	<signature: #(int (*)(int, char *))>
	self subclassResponsibility
]

{ #category : #signatures }
Callback >> intcharstarRetint: callbackContext sp: spAlien [
	<signature: #(int (*)(int, char *))>
	self subclassResponsibility
]

{ #category : #'instance initialization' }
Callback >> signature: signature "<String>" block: aBlock [ "<BlockClosure>"
	| method |
	method := self evaluatorForSignature: signature.
	block := aBlock.
	thunk := (method pragmaAt: #calleepops:)
				ifNil: [FFICallbackThunk newCCall]
				ifNotNil:
					[:pragma|
					FFICallbackThunk newStdCall: (pragma argumentAt: 1)].
	numEvaluatorArgs := (evaluator := method selector) numArgs.
	self addToThunkTable
]

{ #category : #accessing }
Callback >> thunk [
	^thunk
]

{ #category : #evaluation }
Callback >> valueInContext: callbackContext [ "<VMCallbackContext32|VMCallbackContext64> ^<Integer>"
	"Perform the evaluator with arguments derived from the callback context in an order most appropriate for the ABI.
	 On x86 the order is callbackContext, stackPointerAlien.
	 On ``RISCS'' it is callbackContext, integerRegsAlien, stackPointerAlien, floatRegsAlien"
	self subclassResponsibility
]

{ #category : #evaluation }
Callback >> valueWithContext: vmCallbackContext [ "<VMCallbackContext32>" "^<Integer>"
	<legacy>
	^block
		value: (argsProxyClass
					atAddress: vmCallbackContext rawStackp
					dataSize: argsProxyClass dataSize)
		value: vmCallbackContext
]

{ #category : #evaluation }
Callback >> valueWithStackPointer: stackPtr [ "<Integer>" "^<FFICallbackReturnValue>"
	<legacy>
	| resultProxy |
	block
		value: (argsProxyClass atAddress: stackPtr)
		value: (resultProxy := FFICallbackReturnValue new).
	^resultProxy
]

{ #category : #signatures }
Callback >> voidstarvoidstarRetint: callbackContext regs: regsAlien [
	<signature: #(int (*)(const void *, const void *))>
	self subclassResponsibility
]

{ #category : #signatures }
Callback >> voidstarvoidstarRetint: callbackContext sp: spAlien [
	<signature: #(int (*)(const void *, const void *))>
	self subclassResponsibility
]
