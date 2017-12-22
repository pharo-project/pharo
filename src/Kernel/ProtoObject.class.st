"
ProtoObject establishes minimal behavior required of any object in Pharo, even objects that should balk at normal object behavior. 

Generally these are proxy objects designed to read themselves in from the disk, or to perform some wrapper behavior, before responding to a message. 

ProtoObject has no instance variables, nor should any be added.
"
Class {
	#name : #ProtoObject,
	#superclass : #nil,
	#category : #'Kernel-Objects'
}

{ #category : #comparing }
ProtoObject >> == anObject [ 
	"Primitive. Answer whether the receiver and the argument are the same 
	object (have the same object pointer). Do not redefine the message == in 
	any other class! Essential. No Lookup. Do not override in any subclass. 
	See Object documentation whatIsAPrimitive."

	<primitive: 110>
	self primitiveFailed
]

{ #category : #'reflective operations' }
ProtoObject >> basicIdentityHash [
	"Answer a SmallInteger whose value is related to the receiver's identity.
	This method must not be overridden, except by SmallInteger.
	Primitive. Fails if the receiver is a SmallInteger. Essential.
	See Object documentation whatIsAPrimitive.

	Do not override. Use #identityHash unless you really know what you're doing.'"

	<primitive: 75>
	self primitiveFailed
]

{ #category : #'reflective operations' }
ProtoObject >> become: otherObject [ 
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger"

	(Array with: self)
		elementsExchangeIdentityWith:
			(Array with: otherObject)
]

{ #category : #'reflective operations' }
ProtoObject >> cannotInterpret: aMessage [ 
	 "Handle the fact that there was an attempt to send the given message to the receiver but a null methodDictionary was encountered while looking up the message selector.  Hopefully this is the result of encountering a stub for a swapped out class which induces this exception on purpose."

"If this is the result of encountering a swap-out stub, then simulating the lookup in Smalltalk should suffice to install the class properly, and the message may be resent."

	(self class lookupSelector: aMessage selector) ifNotNil:
		["Simulated lookup succeeded -- resend the message."
		^ aMessage sentTo: self].

	"Could not recover by simulated lookup -- it's an error"
	Error signal: 'MethodDictionary fault'.

	"Try again in case an error handler fixed things"
	^ aMessage sentTo: self
]

{ #category : #'class membership' }
ProtoObject >> class [
	"Primitive. Answer the object which is the receiver's class. Essential. See 
	Object documentation whatIsAPrimitive."

	<primitive: 111>
	self primitiveFailed
]

{ #category : #debugging }
ProtoObject >> doOnlyOnce: aBlock [
	"If the 'one-shot' mechanism is armed, evaluate aBlock once and disarm the one-shot mechanism.  To rearm the mechanism, evaluate  'self rearmOneShot' manually."

	(self class environment at: #OneShotArmed ifAbsent: [ true ])
		ifTrue: [ 
			self class environment at: #OneShotArmed put: false.
			aBlock value ]
]

{ #category : #'reflective operations' }
ProtoObject >> doesNotUnderstand: aMessage [

	^ MessageNotUnderstood new 
		message: aMessage;
		receiver: self;
		signal
]

{ #category : #executing }
ProtoObject >> executeMethod: compiledMethod [
	^ self withArgs: #( ) executeMethod: compiledMethod
]

{ #category : #flagging }
ProtoObject >> flag: aSymbol [

	"Send this message, with a relevant symbol as argument, to flag a message for subsequent retrieval.  For example, you might put the following line in a number of messages:
	self flag: #returnHereUrgently
	Then, to retrieve all such messages, browse all senders of #returnHereUrgently."
]

{ #category : #comparing }
ProtoObject >> identityHash [
	"Answer a SmallInteger whose value is related to the receiver's identity.
	 This method must not be overridden, except by SmallInteger.  As of
	 2014, the 32-bit Spur VM has 22 bits of hash and 31-bit SmallIntegers
	 (30 bits + 1 sign bit).  Shifting by 8 will not create large integers.
	
	 Do not override."

	^self basicIdentityHash bitShift: 8
]

{ #category : #testing }
ProtoObject >> ifNil: nilBlock [
	"Return self, or evaluate the block if I'm == nil (q.v.)"

	^ self
]

{ #category : #testing }
ProtoObject >> ifNil: nilBlock ifNotNil: ifNotNilBlock [
	"If the receiver is not nil, pass it as argument to the ifNotNilBlock block. else execute the nilBlock block "
	
	"(nil ifNil: [42] ifNotNil: [:o | o +3 ] ) >>> 42"
	"(3  ifNil: [42] ifNotNil: [:o | o +3 ]) >>> 6"
	
	^ ifNotNilBlock cull: self
]

{ #category : #testing }
ProtoObject >> ifNotNil: ifNotNilBlock [
	"Evaluate the block, unless I'm == nil (q.v.). If the receiver is not nil, pass it as argument to the block."

	"(2 ifNotNil: [ :o | o + 3]) >>> 5"
	"(nil ifNotNil: [:o | o +3 ]) >>> nil"

	^ ifNotNilBlock cull: self
]

{ #category : #testing }
ProtoObject >> ifNotNil: ifNotNilBlock ifNil: nilBlock [ 
	"If the receiver is not nil, pass it as argument to the ifNotNilBlock block. else execute the nilBlock block "

	"(nil ifNotNil: [:o | o +3 ] ifNil: [42]) >>> 42"
	"(3 ifNotNil: [:o | o +3 ] ifNil: [42]) >>> 6"

	^ ifNotNilBlock cull: self
]

{ #category : #initialization }
ProtoObject >> initialize [
	"Subclasses should redefine this method to perform initializations on instance creation" 
]

{ #category : #introspection }
ProtoObject >> instVarsInclude: anObject [
"Answers true if anObject is among my named or indexed instance variables, and false otherwise"

	<primitive: 132>
	1 to: self class instSize do:
		[:i | (self instVarAt: i) == anObject ifTrue: [^ true]].
	1 to: self basicSize do:
		[:i | (self basicAt: i) == anObject ifTrue: [^ true]].
	^ false
]

{ #category : #testing }
ProtoObject >> isNil [
	"Coerces nil to true and everything else to false."

	^false
]

{ #category : #'memory scanning' }
ProtoObject >> nextInstance [
	"Primitive. Answer the next instance after the receiver in the 
	enumeration of all instances of this class. Fails if all instances have been 
	enumerated. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 78>
	^nil
]

{ #category : #'memory scanning' }
ProtoObject >> nextObject [
	"Primitive. Answer the next object after the receiver in the 
	enumeration of all objects. Return 0 when all objects have been 
	enumerated."

	<primitive: 139>
	self primitiveFailed.
]

{ #category : #'pointing to' }
ProtoObject >> pointersTo [
	^self pointersToExcept: #()
]

{ #category : #'pointing to' }
ProtoObject >> pointersToExcept: objectsToExclude [
	"Find all objects in the system that hold a pointer to me, excluding those listed"
	| c pointers objectsToAlwaysExclude |
	Smalltalk garbageCollect.
	pointers := OrderedCollection new.

	SystemNavigation default allObjectsDo: [ :e | (e pointsTo: self) ifTrue: [ pointers add: e ]  ].

	objectsToAlwaysExclude := {
		thisContext.
		thisContext sender.
		thisContext sender sender.
		objectsToExclude.
	}.
	c := thisContext.
	^(pointers removeAllSuchThat: [ :ea |
		(ea == thisContext sender) or: [ "warning: this expression is dependent on closure structure of this method"
			(objectsToAlwaysExclude identityIncludes: ea)
				or: [objectsToExclude identityIncludes: ea ]] ]) asArray
]

{ #category : #'pointing to' }
ProtoObject >> pointsTo: anObject [
	"Answers true if I hold a reference to anObject, or false otherwise
	an object points to a class via the header either directly or indirectly 
	via the compact classes array"

	^ (self instVarsInclude: anObject) or: [ ^self class == anObject]
]

{ #category : #debugging }
ProtoObject >> rearmOneShot [
	"Call this manually to arm the one-shot mechanism; use the mechanism in code by calling
		self doOnlyOnce: <a block>"

	self class environment at: #OneShotArmed put: true

	"self rearmOneShot"

]

{ #category : #'apply primitives' }
ProtoObject >> tryPrimitive: primIndex withArgs: argumentArray [
	"This method is a template that the Smalltalk simulator uses to 
	execute primitives. See Object documentation whatIsAPrimitive."

	<primitive: 118 error: code >
	^ Context primitiveFailTokenFor: code
]

{ #category : #executing }
ProtoObject >> withArgs: argArray executeMethod: compiledMethod [
	"Execute compiledMethod against the receiver and args in argArray"

	<primitive: 188>
	self primitiveFailed
]

{ #category : #comparing }
ProtoObject >> ~~ anObject [
	"Primitive. Answer whether the receiver and the argument are different objects
	(do not have the same object pointer). Do not redefine the message ~~ in 
	any other class! Optional (Assuming == is essential). No Lookup. Do not override in any subclass. 
	See Object documentation whatIsAPrimitive."
	
	<primitive: 169>
	self == anObject
		ifTrue: [^ false]
		ifFalse: [^ true]
]
