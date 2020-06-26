"
I model a temporary variable, similar to Slot and LiteralVariable.

For now I am cread on need and never persisted. (this might change later
to allow me to be annotated with properties.)

I know the method that I come from. If I am a block temp, I record the startPC of that block.
"
Class {
	#name : #TemporaryVariable,
	#superclass : #Variable,
	#instVars : [
		'method',
		'startpc'
	],
	#classVars : [
		'PersistentTemps'
	],
	#category : #'Kernel-Variables'
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
	^self new 
		name: aName;
		method: aBlock method;
		startpc: aBlock startpc		
]

{ #category : #'instance creation' }
TemporaryVariable class >> name: aName context: aContext [
	^aContext isBlock
		ifTrue: [ self name: aName block: aContext closure ]
		ifFalse: [ self name: aName method: aContext method ]
]

{ #category : #'instance creation' }
TemporaryVariable class >> name: aName method: aMethod [
	^self new 
		name: aName;
		method: aMethod
	
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

{ #category : #testing }
TemporaryVariable >> isReferenced [
	^self astNodes isNotEmpty
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

{ #category : #'reflecive api' }
TemporaryVariable >> readInContext: aContext [
	^aContext tempNamed: name
]

{ #category : #accessing }
TemporaryVariable >> startpc [
	^ startpc
]

{ #category : #accessing }
TemporaryVariable >> startpc: anObject [
	startpc := anObject
]

{ #category : #queries }
TemporaryVariable >> usingMethods [
	^{method}
]

{ #category : #'reflecive api' }
TemporaryVariable >> write: aValue inContext: aContext [
	aContext tempNamed: name put: aValue
]
