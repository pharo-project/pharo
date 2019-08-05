"
It is container of all mirror primitives implemented in VM.
Mirror primitives allow to call some primitive on object without sending message to it. It is achived by using receiver as first argument of primitive.

Mirror primitives  violates the principle that each object has sovereign control own state (for example  over the storing of values into its instance variables). But it is essential for the	 debugger. 
	
For details see  Object documentation whatIsAPrimitive.
"
Class {
	#name : #MirrorPrimitives,
	#superclass : #Object,
	#category : #'ReflectionMirrors-Primitives-Base'
}

{ #category : #comparison }
MirrorPrimitives class >> check: anObject identicalTo: anotherObject [
	"Answer whether the first and second arguments are the same object (have the
	 same object pointer) without sending a message to the first argument.  This
	 mimics the action of the VM when it compares two object pointers.  Used to
	 simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 110>
	self primitiveFailed
]

{ #category : #'class relationship' }
MirrorPrimitives class >> classOf: anObject [
	"Primitive. Answer the object which is the receiver's class"

	<primitive: 111>
	self primitiveFailed
]

{ #category : #errors }
MirrorPrimitives class >> errorNotIndexableFor: anObject [
	"Create an error notification that the receiver is not indexable."

	self error: ('Instances of {1} are not indexable' format: {(self classOf: anObject) name})
]

{ #category : #'fields accessing' }
MirrorPrimitives class >> fieldOf: anObject at: anIndex [
	"Answer field at anIndex in order: indexable fields -> fixed fields"

	| fixedSize indexableSize |
	fixedSize := self fixedSizeOf: anObject.
	indexableSize := self indexableSizeOf: anObject.
	
	indexableSize = 0
		ifTrue: [ ^self fixedFieldOf: anObject at: anIndex].
	fixedSize = 0
		ifTrue: [ ^self indexableFieldOf: anObject at: anIndex].
		
	^anIndex > indexableSize
		ifTrue: [self fixedFieldOf: anObject at: anIndex - indexableSize ]
		ifFalse: [ self indexableFieldOf: anObject at: anIndex ]
]

{ #category : #'fields accessing' }
MirrorPrimitives class >> fieldOf: anObject at: anIndex put: newObject [
	"Setting newObject into field at anIndex in order: indexable fields -> fixed fields"

	| fixedSize indexableSize |
	fixedSize := self fixedSizeOf: anObject.
	indexableSize := self indexableSizeOf: anObject.
	
	indexableSize = 0
		ifTrue: [ ^self fixedFieldOf: anObject at: anIndex put: newObject].
	fixedSize = 0
		ifTrue: [ ^self indexableFieldOf: anObject at: anIndex put: newObject].
			
	^anIndex > indexableSize
		ifTrue: [self fixedFieldOf: anObject at: anIndex - indexableSize put: newObject]
		ifFalse: [ self indexableFieldOf: anObject at: anIndex put: newObject]
]

{ #category : #'fields accessing' }
MirrorPrimitives class >> fixedFieldOf: anObject at: anIndex [
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	 variables corresponds to the named instance variables. Fail if the index 
	 is not an Integer or is not the index of a fixed variable"

	<primitive: 73>
	"Access beyond fixed variables."
	^self primitiveFailed
]

{ #category : #'fields accessing' }
MirrorPrimitives class >> fixedFieldOf: anObject at: anIndex put: newValue [ 
	"Primitive. Store a value into a fixed variable in the argument anObject.
	 The numbering of the variables corresponds to the named instance
	 variables.  Fail if the index is not an Integer or is not the index of a
	 fixed variable.  Answer the value stored as the result"

	<primitive: 74>
	anIndex isInteger ifTrue: [
		(anIndex between: 1 and: (self fixedSizeOf: anObject)) 
			ifTrue: [
				(self isObjectReadOnly: anObject) ifTrue: [ 
					^self 
						modificationForbiddenFor: anObject 
						at: anIndex 
						with: newValue
						selector: #fixedFieldOf:at:put:]].
		^self primitiveFail].		
	self errorNonIntegerIndex
]

{ #category : #'fields accessing' }
MirrorPrimitives class >> fixedSizeOf: anObject [
	"Answer the number of inst vars"

	^(self classOf: anObject) instSize
]

{ #category : #hashes }
MirrorPrimitives class >> identityHashOf: anObject [
	"Answer a SmallInteger whose value is related to the receiver's identity.
	This method must not be overridden, except by SmallInteger.
	Primitive. Fails if the receiver is a SmallInteger"

	<primitive: 75>
	self primitiveFailed
]

{ #category : #'fields accessing' }
MirrorPrimitives class >> indexableFieldOf: anObject at: anIndex [ 
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	indexable element in the receiver. Fail if the argument index is not an 
	Integer or is out of bounds. Essential. See Object documentation 
	whatIsAPrimitive. Read the class comment for a discussion about that the fact
	that the index can be a float."

	<primitive: 60>
	anIndex isInteger ifTrue:
		[(self classOf: anObject) isVariable
			ifTrue: [self errorSubscriptBounds: anIndex]
			ifFalse: [self errorNotIndexableFor: anObject]].
		
	self errorNonIntegerIndex
]

{ #category : #'fields accessing' }
MirrorPrimitives class >> indexableFieldOf: anObject at: anIndex put: newValue [
	"Primitive. Assumes receiver is indexable. Store the argument value in 
	the indexable element of the receiver indicated by index. Fail if the 
	index is not an Integer or is out of bounds. Or fail if the value is not of 
	the right type for this kind of collection. Answer the value that was 
	stored"
	<primitive: 61>
	anIndex isInteger ifTrue:
		[(self classOf: anObject) isVariable
			ifTrue: [(anIndex between: 1 and: (self indexableSizeOf: anObject))
					ifTrue: [
						(self isObjectReadOnly: anObject) ifTrue: [
							^self 
								modificationForbiddenFor: anObject 
								at: anIndex 
								with: newValue
								selector: #indexableFieldOf:at:put:].
						^self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: anIndex]]
			ifFalse: [self errorNotIndexableFor: anObject]].
	
	self errorNonIntegerIndex
]

{ #category : #'fields accessing' }
MirrorPrimitives class >> indexableSizeOf: anObject [
	"Answer the number of indexable variables in the argument anObject without sending
	 it a message. This mimics the action of the VM when it fetches an object's variable size"

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0
]

{ #category : #'write barrier' }
MirrorPrimitives class >> isObjectReadOnly: anObject [
	<primitive: 163 error: ec>
	^ self primitiveFail
]

{ #category : #'write barrier' }
MirrorPrimitives class >> makeObject: anObject readOnly: aBoolean [
	<primitive: 164 error: ec>
	^ self primitiveFail
]

{ #category : #errors }
MirrorPrimitives class >> modificationForbiddenFor: anObject at: fieldIndex with: newObject selector: selector [
	
	^(MirrorModificationForbidden 
		for: anObject 
		at: fieldIndex 
		with: newObject 
		retrySelector: selector) signal
]

{ #category : #printing }
MirrorPrimitives class >> print: anObject [
	
	^String streamContents: [ :s |	self print: anObject on: s] limitedTo: 32.

]

{ #category : #printing }
MirrorPrimitives class >> print: anObject on: aStream [
	| title |
	title := (self classOf: anObject) name.
	aStream 
		nextPutAll: ((title at: 1) isVowel ifTrue: ['an '] ifFalse: ['a ']).
	aStream nextPutAll: title
]

{ #category : #'class relationship' }
MirrorPrimitives class >> setClass: classObject to: anObject [
	"Change the class of anObject to classObject"
	
	<primitive: 160 error: ec>
	(self isObjectReadOnly: anObject) 
		ifTrue: [ ^ (ModificationForbidden 
							for: anObject
							at: nil
							with: classObject
							retrySelector: #setClass:to:) signal ]. 
	self primitiveFailed
]

{ #category : #'fields accessing' }
MirrorPrimitives class >> sizeOf: anObject [
	"Answer the number of inst vars + number of indexible fields"

	^(self fixedSizeOf: anObject) + (self indexableSizeOf: anObject)
]

{ #category : #'message performing' }
MirrorPrimitives class >> withReceiver: receiver andArguments: argArray execute: compiledMethod [
	"Execute compiledMethod against the receiver and the arguments in argArray"

	<primitive: 188>
	^self primitiveFailed
]

{ #category : #'message performing' }
MirrorPrimitives class >> withReceiver: anObject perform: selector withArguments: argArray [
	"Send the selector, aSymbol, to the receiver with arguments in argArray.
	Fail if the number of arguments expected by the selector 
	does not match the size of argArray.
	Primitive. Optional. See Object documentation whatIsAPrimitive."
	
	<primitive: 84 error: error>
	self primitiveFailed
]

{ #category : #'message performing' }
MirrorPrimitives class >> withReceiver: receiver perform: selector withArguments: argArray inSuperclass: lookupClass [
	"NOTE:  This is just like perform:withArguments:, except that
	the message lookup process begins, not with the receivers's class,
	but with the supplied superclass instead.  It will fail if lookupClass
	cannot be found among the receiver's superclasses"

	<primitive: 100 error: error>
	selector isSymbol
		ifFalse: [ ^ self error: 'selector argument must be a Symbol' ].
	(self classOf: argArray) == Array
		ifFalse: [ ^ self error: 'argArray must be an Array' ].
	(selector numArgs = self indexableSizeOf: argArray)
		ifFalse: [ ^ self error: 'incorrect number of arguments' ].
	((self classOf: receiver) includesBehavior: lookupClass)
		ifFalse: [ ^ self error: 'lookupClass is not in my inheritance chain' ].
	self primitiveFailed
]

{ #category : #'message performing' }
MirrorPrimitives class >> withReceiver: receiver tryPrimitive: number withArguments: args [
	"This method is a template that the Smalltalk simulator uses to 
	execute primitives"

	<primitive: 118 error: code >
	^self primitiveFailed
]

{ #category : #help }
MirrorPrimitives >> seeClassSide [
]
