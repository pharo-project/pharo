"
I represent a pair of associated objects--a key and a value. My instances can serve as entries in a dictionary.


Implementation notes: 
Note that hash is not redefined even if the = was redefined because Association>>hash may cause extreme slowdowns in compiling Unicode methods. Association>>hash does not need to hash the value; it's slow and useless.


"
Class {
	#name : #Association,
	#superclass : #LookupKey,
	#instVars : [
		'value'
	],
	#category : #'Collections-Support-Associations'
}

{ #category : #'instance creation' }
Association class >> key: newKey value: newValue [
	"Answer an instance of me with the arguments as the key and value of 
	the association."

	^self basicNew key: newKey value: newValue
]

{ #category : #comparing }
Association >> = anAssociation [

	^ super = anAssociation and: [value = anAssociation value]
]

{ #category : #testing }
Association >> analogousCodeTo: anAssociation [
	^ self = anAssociation
]

{ #category : #'self evaluating' }
Association >> isSelfEvaluating [
	^ self class == Association and: [self key isSelfEvaluating and: [self value isSelfEvaluating]]
]

{ #category : #accessing }
Association >> key: aKey value: anObject [ 
	"Store the arguments as the variables of the receiver."

	key := aKey.
	value := anObject
]

{ #category : #comparing }
Association >> literalEqual: otherLiteral [
	"Answer true if the receiver and otherLiteral represent the same literal.
	Variable bindings are literally equals only if identical.
	This is how variable sharing works, by preserving identity and changing only the value."
	^self == otherLiteral
]

{ #category : #testing }
Association >> needsFullDefinition [
	"Implemented here so we can use simple Associations as Bindings instead of LiteralVariable
	subclasses"
	^false
]

{ #category : #printing }
Association >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: '->'.
	value printOn: aStream
]

{ #category : #printing }
Association >> storeOn: aStream [
	"Store in the format (key->value)"
	aStream nextPut: $(.
	key storeOn: aStream.
	aStream nextPutAll: '->'.
	value storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #evaluating }
Association >> value [
	"Answer the value of the receiver."

	^value
]

{ #category : #accessing }
Association >> value: anObject [ 
	"Store the argument, anObject, as the value of the receiver."

	value := anObject
]
