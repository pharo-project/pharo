"
I am a dictionary holding only weakly on my values. Clients may expect to get a nil value for any object they request.
"
Class {
	#name : #WeakValueDictionary,
	#superclass : #Dictionary,
	#category : #'Collections-Weak-Dictionaries'
}

{ #category : #adding }
WeakValueDictionary >> add: anAssociation [
	self at: anAssociation key put: anAssociation value.
	^ anAssociation
]

{ #category : #enumerating }
WeakValueDictionary >> associationsDo: aBlock [
	"Evaluate aBlock for each of the receiver's elements (key/value associations)."

	tally = 0 ifTrue: [ ^ self ].
	array do: [ :each | each value ifNotNil: [ aBlock value: each key -> each value enclosedSetElement ] ]
]

{ #category : #adding }
WeakValueDictionary >> at: key ifAbsent: aBlock [
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	^ (array at: (self findElementOrNil: key)) value
			ifNil: aBlock
			ifNotNil: [ :value | value enclosedSetElement ]
]

{ #category : #adding }
WeakValueDictionary >> at: key ifPresent: aBlock [
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the given block optionally with the value associated
	with the key.
	Otherwise, answer nil."

	^ (array at: (self findElementOrNil: key)) value
			ifNotNil: [ :assoc | aBlock cull: assoc value enclosedSetElement ]
]

{ #category : #accessing }
WeakValueDictionary >> at: key put: anObject [
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject."

	| index element |
	index := self findElementOrNil: key.
	element := array at: index.
	element == nil
		ifTrue: [ self atNewIndex: index put: (WeakValueAssociation key: key value: anObject asSetElement) ]
		ifFalse: [ element value: anObject asSetElement ].
	^ anObject
]

{ #category : #accessing }
WeakValueDictionary >> size [
	| count |
	count := 0.
	self valuesDo: [ :each | count := count + 1 ].
	^ count
]

{ #category : #enumerating }
WeakValueDictionary >> valuesDo: aBlock [
	"See comments in Dictionary>>valuesDo:.  The code keeps
	a reference to the value to facilitate debugging"

	tally = 0 ifTrue: [ ^ self ].
	1 to: array size do: [ :eachIndex | 
		| eachAssociation eachValue |
		eachAssociation := array at: eachIndex.
		nil == eachAssociation value ifFalse: [ 
			eachValue := eachAssociation value enclosedSetElement.
			aBlock value: eachValue ] ]
]
