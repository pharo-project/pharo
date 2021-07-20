"
I'm a reference. I can be a Link or a Figure. Later i could be a video for example.
"
Class {
	#name : #PRReference,
	#superclass : #PRDocumentGroup,
	#traits : 'PRTParametrizable',
	#classTraits : 'PRTParametrizable classTrait',
	#instVars : [
		'reference',
		'parameters'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #testing }
PRReference class >> isValidReference: aString [
	self subclassResponsibility
]

{ #category : #private }
PRReference class >> localFileOrExternalReference: aString [
	| localFileIndicator |
	localFileIndicator := 'file://'.
	^ (aString beginsWith: localFileIndicator)
			ifTrue: [ aString allButFirst: localFileIndicator size ]
			ifFalse: [ aString ]
]

{ #category : #'instance creation' }
PRReference class >> reference: aString [
	^ self new
		reference: aString;
		yourself
]

{ #category : #comparing }
PRReference >> = anObject [
	^ super = anObject and: [ self reference = anObject reference and: [ self parameters = anObject parameters ] ]
]

{ #category : #visiting }
PRReference >> accept: aVisitor [
	<ignoreForCoverage "This method is ignored for test coverage because it is overriden in all subclasses and these subclasses don't need to do a super-send.">
	^ aVisitor visitReference: self
]

{ #category : #'accessing - dynamic' }
PRReference >> alias [
	"Answer the alias of the receiver or an empty string if none."

	^ self text
]

{ #category : #testing }
PRReference >> hasAlias [
	^ self hasChildren
]

{ #category : #testing }
PRReference >> hasAnchor [
	^ false
]

{ #category : #comparing }
PRReference >> hash [
	^ super hash bitXor: self reference hash
]

{ #category : #initialization }
PRReference >> initialize [
	super initialize.
	self parameters: OrderedDictionary new
]

{ #category : #accessing }
PRReference >> localFileOrExternalReference [
	^ self class localFileOrExternalReference: self reference
]

{ #category : #'accessing - parameters' }
PRReference >> parameters [
	"Answer an array of parameters."

	^ parameters
]

{ #category : #accessing }
PRReference >> parameters: aCollection [
	parameters := aCollection
]

{ #category : #copying }
PRReference >> postCopy [
	super postCopy.
	self parameters: (self parameters
		collect: [ :each | each copy ])
]

{ #category : #accessing }
PRReference >> reference [
	"Answer the reference of the receiver."

	^ reference
]

{ #category : #accessing }
PRReference >> reference: aString [
	reference := aString
]

{ #category : #'accessing - dynamic' }
PRReference >> text [
	^ super text ifEmpty: [ self reference ] ifNotEmpty: #yourself
]

{ #category : #accessing }
PRReference >> title [
	"Answer a title of the reference, this is should never be nil."

	^ self reference
]
