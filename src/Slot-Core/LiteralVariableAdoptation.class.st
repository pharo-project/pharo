Class {
	#name : #LiteralVariableAdoptation,
	#superclass : #Variable,
	#instVars : [
		'value'
	],
	#category : #'Slot-Core-Variables'
}

{ #category : #'instance creation' }
LiteralVariableAdoptation class >> key: aKey [ 
	"Answer an instance of me with the argument as the lookup up."

	^self basicNew key: aKey
]

{ #category : #'instance creation' }
LiteralVariableAdoptation class >> key: newKey value: newValue [
	"Answer an instance of me with the arguments as the key and value of 
	the association."

	^self basicNew key: newKey value: newValue
]

{ #category : #comparing }
LiteralVariableAdoptation >> < aLookupKey [ 
	"Refer to the comment in Magnitude|<."

	^name < aLookupKey key
]

{ #category : #comparing }
LiteralVariableAdoptation >> = anAssociation [

	^ super = anAssociation and: [value = anAssociation value]
]

{ #category : #comparing }
LiteralVariableAdoptation >> hash [
	"Hash is reimplemented because = is implemented."

	^name hash
]

{ #category : #accessing }
LiteralVariableAdoptation >> key [ 
	^name
]

{ #category : #accessing }
LiteralVariableAdoptation >> key: anObject [ 
	name := anObject
]

{ #category : #accessing }
LiteralVariableAdoptation >> key: aKey value: anObject [ 
	"Store the arguments as the variables of the receiver."

	name := aKey.
	value := anObject
]

{ #category : #printing }
LiteralVariableAdoptation >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: '->'.
	value printOn: aStream
]

{ #category : #printing }
LiteralVariableAdoptation >> storeOn: aStream [
	"Store in the format (key->value)"
	aStream nextPut: $(.
	name storeOn: aStream.
	aStream nextPutAll: '->'.
	value storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #accessing }
LiteralVariableAdoptation >> value [
	^ value
]

{ #category : #accessing }
LiteralVariableAdoptation >> value: anObject [
	value := anObject
]
