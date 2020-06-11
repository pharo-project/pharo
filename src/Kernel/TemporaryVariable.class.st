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
	^ super ensureProperties
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
