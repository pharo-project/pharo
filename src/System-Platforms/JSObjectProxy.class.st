Class {
	#name : #JSObjectProxy,
	#superclass : #Object,
	#classVars : [
		'CallbackProcess',
		'CallbackSemaphore'
	],
	#category : #'System-Platforms-Web'
}

{ #category : #'as yet unclassified' }
JSObjectProxy class >> callbackProcess [
	[true] whileTrue: [
		CallbackSemaphore wait.
		[self handleCallback] fork].

]

{ #category : #'as yet unclassified' }
JSObjectProxy class >> evaluate: source [
	^(JS Function new: 'return ', source, ';') call
]

{ #category : #'as yet unclassified' }
JSObjectProxy class >> examples [
	| comment |
	"Create symbols in advance"
	('alert: console log: document getElementsByTagName: navigator Object keys: Function length setTimeout:ms: createElement: appendChild: jQuery: hide: show: css: click: appendTo:'
		findTokens: ' ') do: [:s | s asSymbol].
	comment := self organization classComment asString.
	^ comment copyFrom: (comment indexOf: $") to: comment size.

]

{ #category : #'as yet unclassified' }
JSObjectProxy class >> handleCallback [
	| block |
	
	block := self primGetActiveCallbackBlock.
	block
		ifNil: [
			Transcript
				cr;
				nextPutAll: 'active JavaScript callback block got nulled';
				endEntry]
		ifNotNil: [
			| args result |
			
			args := self primGetActiveCallbackArgs.
			[result := block valueWithArguments: args] ifError: [:error :receiver |
				ActiveWorld halt: error asString.
				result := JS Error: error asString].
			
			self primReturnFromCallback: result]

]

{ #category : #'as yet unclassified' }
JSObjectProxy class >> initCallbacks [
	CallbackProcess ifNotNil: [CallbackProcess terminate. CallbackProcess := nil].
	CallbackSemaphore := Semaphore new.
	self primInitCallbacks: (Smalltalk registerExternalObject: CallbackSemaphore).
	CallbackProcess := [self callbackProcess] newProcess
		priority: Processor lowIOPriority; resume.

]

{ #category : #'as yet unclassified' }
JSObjectProxy class >> initialize [
	"Create the JS global"
	Smalltalk at: #JS put: self basicNew.
	"If we have the plugin, show workspace"
	[JS window] ifError: [:err :rcvr | ^self].
	Smalltalk isMorphic
		ifTrue: [self openExamples]
		ifFalse: [[self openExamples] fork].

]

{ #category : #'as yet unclassified' }
JSObjectProxy class >> new [
	self error: 'Use "JS Object new" to create a new JavaScript object'.

]

{ #category : #'as yet unclassified' }
JSObjectProxy class >> openExamples [
	Workspace new
		contents: 'Besides running regular Squeak images, SqueakJS can directly use JavaScript. It can interact with the DOM, access JavaScript libraries, and use Smalltalk code to create an interactive HTML interface. Try these examples:

', self examples;
		openLabel: 'JSBridge'.

]

{ #category : #'as yet unclassified' }
JSObjectProxy class >> primGetActiveCallbackArgs [
	<primitive: 117> #(JavaScriptPlugin primitiveGetActiveCallbackArgs 0 0) at: 1.
	^ self primitiveFailed

]

{ #category : #'as yet unclassified' }
JSObjectProxy class >> primGetActiveCallbackBlock [
	<primitive: 117> #(JavaScriptPlugin primitiveGetActiveCallbackBlock 0 0) at: 1.
	^nil

]

{ #category : #'as yet unclassified' }
JSObjectProxy class >> primGetError [
	<primitive: 117> #(JavaScriptPlugin primitiveGetError 0 0) at: 1.
	^ nil
]

{ #category : #'as yet unclassified' }
JSObjectProxy class >> primInitCallbacks: semaIndex [
	<primitive: 117> #(JavaScriptPlugin primitiveInitCallbacks 0 0) at: 1.
	^ self primitiveFailed

]

{ #category : #'as yet unclassified' }
JSObjectProxy class >> primReturnFromCallback: returnValue [
	<primitive: 117> #(JavaScriptPlugin primitiveReturnFromCallback 0 0) at: 1.
	(self primGetError = 'No active callback') ifFalse: [self primitiveFailed]
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> asArray [
	| array |
	
	array := Array new: self length.
	
	1
		to: self length
		do: [:index |
			array
				at: index
				put: (self at: index - 1)].
			
	^array
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> asJSArgument [
	^ self

]

{ #category : #'as yet unclassified' }
JSObjectProxy >> asString [
	"Convert me to a string"
	<primitive: 117> #(JavaScriptPlugin primitiveAsString 0 0) at: 1.
	^ self primitiveFailed

]

{ #category : #'as yet unclassified' }
JSObjectProxy >> asSymbol [
	^self asString asSymbol
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> at: aKey [
	"get a property"
	| error |
	<primitive: 117> #(JavaScriptPlugin primitiveAt 0 0) at: 1.
	(error := self primGetError)
		ifNotNil: [^ self error: error].
	^ self primitiveFailed

]

{ #category : #'as yet unclassified' }
JSObjectProxy >> at: aKey put: aValue [
	"set a property"
	| error |
	<primitive: 117> #(JavaScriptPlugin primitiveAtPut 0 0) at: 1.
	^ self with: aValue retry: [:val | self at: aKey put: val]

]

{ #category : #'as yet unclassified' }
JSObjectProxy >> break: message [
	self ifConnected: [
		self console log: message.
		
		self
			alert: message;
			debugger]
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> constructor [
	^self at: 'constructor'
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> counterpart [
	"Answer an instance of a Smalltalk class that corresponds to my constructor, and which provides API documentation and live interaction."

	^JSObject counterpartFor: self
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> debugger [
	(JS Function new: 'debugger') call
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> doesNotUnderstand: aMessage [
	"Call a function, or get/set an existing property. The function name / property name is the message selector up to the first colon. If the function name is 'new', create a new instance and call the constructor with args."
	<primitive: 117> #(JavaScriptPlugin primitiveDoUnderstand 0 0) at: 1.
	^self with: aMessage arguments retry: [:args |
		self doesNotUnderstand: (Message selector: aMessage selector arguments: args)]

]

{ #category : #'as yet unclassified' }
JSObjectProxy >> evaluate: source [
	| result |
	
	JS at: #messageReceiver put: self.
	(JS messageReceiver)
		at: #messageFunction
		put: (JS Function new: source).
	result := JS messageReceiver messageFunction.
	(JS Function new: 'delete messageReceiver.messageFunction') call.
	(JS Function new: 'delete messageReceiver') call.
		
	^result
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> foobly [
	<primitive: 'primitiveFoobly' module: 'JavaScriptPlugin'>
	^ nil

]

{ #category : #'as yet unclassified' }
JSObjectProxy >> ifConnected: connected [
	"If SqueakJS is running my object memory, evaluate aBlockClosure."

	^self
		ifConnected: connected
		ifNotConnected: [nil]
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> ifConnected: connected ifNotConnected: notConnected [
	[self top]
		on: Error
		do: [:exception | ^notConnected value].

	^connected value
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> isConnected [
	"If SqueakJS is running my object memory, evaluate aBlockClosure."

	^self
		ifConnected: [true]
		ifNotConnected: [false]
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> isIdenticalTo: proxy [
	| object |
	
	object := JS Object new.

	object
		at: #firstThing put: self;
		at: #secondThing put: proxy;
		at: #comparison put: (JS Function new: 'return this.firstThing == this.secondThing').
		
	^object comparison
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> name [
	^self evaluate: 'return this.name'
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> newWithParameters: parameters [
	| parametersJS |
	
	parametersJS := JS Array new.
	parameters do: [:parameter | parametersJS push: parameter asJSArgument].
	^(self primitiveNewWithParameters: parametersJS) counterpart
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> primGetError [
	<primitive: 117> #(JavaScriptPlugin primitiveGetError 0 0) at: 1.
	^ nil

]

{ #category : #'as yet unclassified' }
JSObjectProxy >> primitiveNewWithParameters: parameters [
	<primitive: 117> #(JavaScriptPlugin primitiveNewWithParameters 0 0) at: 1.
	^ self primitiveFailed

]

{ #category : #'as yet unclassified' }
JSObjectProxy >> primitiveSqueakAsJSObject: object [
	<primitive: 117> #(JavaScriptPlugin primitiveSqueakAsJSObject 0 0) at: 1.
	^ self primitiveFailed
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> printOn: aStream [
	[aStream nextPutAll: self asString]
		ifError: [:err :rcvr | ^ super printOn: aStream].

]

{ #category : #'as yet unclassified' }
JSObjectProxy >> removeKey: key [
	^self evaluate: 'delete this.', key
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> stringify [
	^(JS window JSON stringify: self) asString
]

{ #category : #'as yet unclassified' }
JSObjectProxy >> typeof [
	"Answer my jsObject's type (a string)"
	<primitive: 117> #(JavaScriptPlugin primitiveTypeof 0 0) at: 1.
	^ self primitiveFailed

]

{ #category : #'as yet unclassified' }
JSObjectProxy >> with: argument retry: retryBlock [
	| error |
	(error := self primGetError) ifNil: [^ self error: 'JSBridge error'].
	(error beginsWith: 'asJSArgument') ifTrue: [
		^retryBlock value: argument asJSArgument].
	(error beginsWith: 'CallbackSemaphore') ifTrue: [
		self class initCallbacks.
		^retryBlock value: argument].
	self error: error.

]
