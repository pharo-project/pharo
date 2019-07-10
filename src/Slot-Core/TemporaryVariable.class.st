"
I model a temporary variable, similar to Slot and LiteralVariable.

For now I am cread on need and never persisted. (this might change later
to allow me to be annotated with properties.)

I know the method that I come from. If I am a block temp, I record the startPC of that block.
"
Class {
	#name : #TemporaryVariable,
	#superclass : #Object,
	#instVars : [
		'name',
		'method',
		'startpc'
	],
	#classVars : [
		'PersistentTemps',
		'Properties'
	],
	#category : #'Slot-Core-Variables'
}

{ #category : #'class initialization' }
TemporaryVariable class >> initialize [
	Properties := Properties
		ifNil: [ WeakIdentityKeyDictionary new ]
		ifNotNil: [ (WeakIdentityKeyDictionary newFrom: Properties) rehash ].
	PersistentTemps ifNil: [ PersistentTemps := Set new ]
]

{ #category : #'instance creation' }
TemporaryVariable class >> name: aName block: aBlock [
	^(self new 
		name: aName;
		method: aBlock method;
		startpc: aBlock startpc) yourselfOrSaved.		
]

{ #category : #'instance creation' }
TemporaryVariable class >> name: aName context: aContext [
	^aContext isBlock
		ifTrue: [ self name: aName block: aContext closure ]
		ifFalse: [ self name: aName method: aContext method ]
]

{ #category : #'instance creation' }
TemporaryVariable class >> name: aName method: aMethod [
	^(self new 
		name: aName;
		method: aMethod) yourselfOrSaved
	
]

{ #category : #comparing }
TemporaryVariable >> = other [
	self == other
		ifTrue: [ ^ true ].
	^ (self species == other species) 
			and: [ name = other name 
			and: [ method = other method
			and: [ startpc = other startpc ] ] ]
]

{ #category : #properties }
TemporaryVariable >> ensureProperties [
	method saveTemp: self.
	^ Properties at: self ifAbsentPut: WeakKeyDictionary new
]

{ #category : #properties }
TemporaryVariable >> hasProperty: aKey [
	"Test if the property aKey is present."
	^self properties 
		ifNil: [false]
		ifNotNil: [:prop | prop includesKey: aKey]
]

{ #category : #comparing }
TemporaryVariable >> hash [

	^ (name hash
		bitXor: method hash)
		bitXor: (startpc ifNil: [0])
]

{ #category : #testing }
TemporaryVariable >> isFromBlock [
	^startpc isNotNil
]

{ #category : #accessing }
TemporaryVariable >> method [
	^ method
]

{ #category : #accessing }
TemporaryVariable >> method: anObject [
	method := anObject
]

{ #category : #accessing }
TemporaryVariable >> name [
	^ name
]

{ #category : #accessing }
TemporaryVariable >> name: anObject [
	name := anObject
]

{ #category : #properties }
TemporaryVariable >> properties [
	^ Properties at: self ifAbsent: nil
]

{ #category : #properties }
TemporaryVariable >> propertyAt: propName [
	^ self
		propertyAt: propName
		ifAbsent: [ nil ]
]

{ #category : #properties }
TemporaryVariable >> propertyAt: propName ifAbsent: aBlock [
	self properties ifNil: [^aBlock value].
	^ self properties
		at: propName
		ifAbsent: aBlock
]

{ #category : #properties }
TemporaryVariable >> propertyAt: aKey ifAbsentPut: aBlock [
	"Answer the property associated with aKey or, if aKey isn't found store the result of evaluating aBlock as new value."
	
	^ self propertyAt: aKey ifAbsent: [ self propertyAt: aKey put: aBlock value ]
]

{ #category : #properties }
TemporaryVariable >> propertyAt: propName put: propValue [
	^ self ensureProperties
		at: propName
		put: propValue
]

{ #category : #'reflecive api' }
TemporaryVariable >> readInContext: aContext [
	^aContext tempNamed: name
]

{ #category : #properties }
TemporaryVariable >> removePropertiesIfEmpty [
	^ Properties at: self ifPresent: [ :dict |
		dict ifEmpty: [ 
			method removeSavedTemp: self.
			Properties removeKey: self ] ]
]

{ #category : #properties }
TemporaryVariable >> removeProperty: propName [
	^ self
		removeProperty: propName
		ifAbsent: [ nil ]
]

{ #category : #properties }
TemporaryVariable >> removeProperty: propName ifAbsent: aBlock [
	| property |
	self properties ifNil: [^aBlock value].
	property := self properties
		removeKey: propName
		ifAbsent: aBlock.
	self removePropertiesIfEmpty.
	^ property
]

{ #category : #accessing }
TemporaryVariable >> startpc [
	^ startpc
]

{ #category : #accessing }
TemporaryVariable >> startpc: anObject [
	startpc := anObject
]

{ #category : #'reflecive api' }
TemporaryVariable >> write: aValue InContext: aContext [
	^aContext tempNamed: name put: aValue
]

{ #category : #'saved temps' }
TemporaryVariable >> yourselfOrSaved [
	method savedTemps ifNil: [ ^self  ].	
	^method savedTemps 
		detect:  	[ :each | each = self ]
		ifFound: [ :found | found ]
		ifNone: 	[ self ]
]
