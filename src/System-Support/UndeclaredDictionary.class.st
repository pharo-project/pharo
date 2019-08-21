"
I am a Dictionary to store undeclared global variables. 

I act as a WeakValueDictionary but I do not wrap my values into wrapper because I don't need to manage nils.


"
Class {
	#name : #UndeclaredDictionary,
	#superclass : #WeakValueDictionary,
	#category : #'System-Support'
}

{ #category : #enumerating }
UndeclaredDictionary >> associationsDo: aBlock [
	"Evaluate aBlock for each of the receiver's elements (key/value associations)."

	tally = 0 ifTrue: [ ^ self ].
	array do: [ :each | each value ifNotNil: [ :value | aBlock value: each key -> value ] ]
]

{ #category : #enumerating }
UndeclaredDictionary >> at: key ifAbsent: aBlock [
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	^ (array at: (self findElementOrNil: key)) value ifNil: aBlock
]

{ #category : #adding }
UndeclaredDictionary >> at: key ifPresent: aBlock [
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the given block optionally with the value associated
	with the key.
	Otherwise, answer nil."

	^ (array at: (self findElementOrNil: key)) value ifNotNil: [ :value | aBlock cull: value ]
]

{ #category : #accessing }
UndeclaredDictionary >> at: key put: anObject [
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject."

	| index element |
	index := self findElementOrNil: key.
	element := array at: index.
	element == nil
		ifTrue: [ self atNewIndex: index put: (WeakValueAssociation key: key value: anObject) ]
		ifFalse: [ element value: anObject ].
	^ anObject
]

{ #category : #testing }
UndeclaredDictionary >> includesKey: aKey [
	self at: aKey ifAbsent: [ ^ false ].
	^ true
]

{ #category : #enumerating }
UndeclaredDictionary >> valuesDo: aBlock [
	tally = 0 ifTrue: [ ^ self ].
	1 to: array size do: [ :eachIndex | 
		| eachAssociation eachValue |
		eachAssociation := array at: eachIndex.
		nil == eachAssociation value
			ifFalse: [ eachValue := eachAssociation value.
				aBlock value: eachValue ] ]
]
