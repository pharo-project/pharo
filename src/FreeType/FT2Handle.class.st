"
handle holds a (typically 32-bit) pointer to an externally managed object.
"
Class {
	#name : #FT2Handle,
	#superclass : #Object,
	#instVars : [
		'handle'
	],
	#classVars : [
		'DestroyMutex',
		'Registry',
		'Session'
	],
	#pools : [
		'FT2Constants'
	],
	#category : 'FreeType-Base'
}

{ #category : #'system startup' }
FT2Handle class >> clearRegistry [
	self flag: #todo. "This is a WORKAROUND to ensure all handle instances are niled on startup 
	(they are external memory, so they need to be re-created, and for some reason not all handles 
	 are registered to clean up... not good)"
	self allSubInstancesDo: [ :each | each beNull ].
	self registry
		"do: [ :each | each beNull ];"
		removeAll.
]

{ #category : #'private-handle registry' }
FT2Handle class >> deregister: anFT2Handle [

	self registry remove: anFT2Handle ifAbsent: nil.
]

{ #category : #private }
FT2Handle class >> destroyMutex [
	^ DestroyMutex ifNil: [ DestroyMutex := Mutex new ]
]

{ #category : #'error reporting' }
FT2Handle class >> errorCode [
	<primitive: 'primitiveErrorCode' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #'error reporting' }
FT2Handle class >> errorString [
	<primitive: 'primitiveErrorString' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #initialize-release }
FT2Handle class >> initialize [
	SessionManager default 
		registerGuiClassNamed: self name
	
]

{ #category : #'error reporting' }
FT2Handle class >> moduleErrorCode [
	<primitive: 'primitiveModuleErrorCode' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #'private-handle registry' }
FT2Handle class >> register: aHandle [
	(self registry includes: aHandle) ifTrue: [ ^ self ]. 
	self registry add: aHandle
]

{ #category : #'private-handle registry' }
FT2Handle class >> registry [
	^Registry ifNil: [ Registry := WeakRegistry new]
]

{ #category : #'system startup' }
FT2Handle class >> startUp: isImageStarting [
	isImageStarting ifFalse: [ ^ self ].
	
	self clearRegistry.
	"update session"
	Session := Smalltalk session.
]

{ #category : #comparing }
FT2Handle >> = anObject [
	^ (self class == anObject class) and: [ handle = anObject handle ]
]

{ #category : #private }
FT2Handle >> beNull [
	handle := nil.
]

{ #category : #private }
FT2Handle >> destroyHandle [
	"remove receiver from finalization registry, so #finalize won't try to free handle again" 
	self class deregister: self. 

	self pvtDestroyHandle.

]

{ #category : #'error handling' }
FT2Handle >> errorCode [
	<primitive: 'primitiveErrorCode' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #'error handling' }
FT2Handle >> errorString [
	<primitive: 'primitiveErrorString' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #finalization }
FT2Handle >> finalize [
	"If session has changed, and startup is not yet done, we do not attempt 
	to free external resources"
	Session == Smalltalk session ifFalse: [ ^ self ].
	self pvtDestroyHandle.

]

{ #category : #private }
FT2Handle >> handle [
	^handle
]

{ #category : #comparing }
FT2Handle >> hash [
	^ handle hash
]

{ #category : #initialization }
FT2Handle >> initialize [
	self shouldNotImplement.
]

{ #category : #testing }
FT2Handle >> isValid [
	^ handle notNil and: [ handle anySatisfy: [ :each | each isZero not ] ]
]

{ #category : #private }
FT2Handle >> primDestroyHandle [
	self subclassResponsibility
]

{ #category : #'error handling' }
FT2Handle >> primitiveFailed [
	^self primitiveFailed: 'Freetype2 primitive failed'
]

{ #category : #'error handling' }
FT2Handle >> primitiveFailed: aString [
	^FT2Error new
		signal: aString
]

{ #category : #printing }
FT2Handle >> printOn: aStream [
	| handleHex |
	super printOn: aStream.
	handle ifNil: [ ^aStream nextPutAll: '<nil>' ].
	handleHex := (handle unsignedLongAt: 1 bigEndian: EndianDetector isBigEndian) printStringHex.
	aStream nextPutAll: '<0x'; nextPutAll: handleHex; nextPut: $>.
]

{ #category : #private }
FT2Handle >> pvtDestroyHandle [
	"This should only be sent from the finalizer. 
	 It runs inside a mutex because in strange cases it can happen that this is executed twice, 
	 causing a primitiveFailed to be raised."
	self class destroyMutex critical: [ | handleToRelease |
		self isValid ifFalse: [ ^self ].
		handleToRelease := self handle copy.
		self primDestroyHandle.
		"TODO this is bad, but it will prevent duplicated handles to be released"
		FT2Handle allSubInstancesDo: [ :each | 
			(handleToRelease = each handle) ifTrue: [ each beNull ] ] ]
]

{ #category : #validation }
FT2Handle >> validate [
]
