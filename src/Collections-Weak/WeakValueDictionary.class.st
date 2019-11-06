"
I am a dictionary holding only weakly on my values. This mean that as long as my values are referenced (via a strong reference) by other objects, they will stay but in case no object is referencing them during a garbage collection, then my value will vanish and I will return nil instead.
Clients may expect to get a nil value for any object they request since they can be garbaged collected.

Implementation details:

To store keys and values I am using a WeakValueAssociation. This association has a key and a value.
The key is the key the user is giving me, but if the user gives me a nil as value, I wrap it into a CollectionElement. This is explained because I need to do a distinction between nil values given by the user and nil values created by the garbage collection.
When the value of a WeakValueAssociation is a collection element wrapper on nil, then it means the user directly gave us a nil. In case the value of the WeakValueAssociation is nil, it means that we originally had a value that was garbaged collected.
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

{ #category : #adding }
WeakValueDictionary >> associationAt: key ifAbsent: aBlock [
	"Answer the association with the given key.
	If the key is not found, return the result of evaluating aBlock."

	^ (array at: (self findElementOrNil: key))
		ifNil: [ aBlock value ]
		ifNotNil: [ :assoc | assoc key -> assoc value enclosedElement ]
]

{ #category : #enumerating }
WeakValueDictionary >> associationsDo: aBlock [
	"Evaluate aBlock for each of the receiver's elements (key/value associations)."

	tally = 0 ifTrue: [ ^ self ].
	array do: [ :each | each value ifNotNil: [ :value | aBlock value: each key -> value enclosedElement ] ]
]

{ #category : #adding }
WeakValueDictionary >> at: key ifAbsent: aBlock [
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	^ (array at: (self findElementOrNil: key)) value
			ifNil: aBlock
			ifNotNil: [ :value | value enclosedElement ]
]

{ #category : #adding }
WeakValueDictionary >> at: key ifPresent: aBlock [
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the given block optionally with the value associated
	with the key.
	Otherwise, answer nil."

	^ (array at: (self findElementOrNil: key)) value ifNotNil: [ :value | aBlock cull: value enclosedElement ]
]

{ #category : #accessing }
WeakValueDictionary >> at: key put: anObject [
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject."

	| index |
	index := self findElementOrNil: key.
	(array at: index)
		ifNil: [ self atNewIndex: index put: (WeakValueAssociation key: key value: anObject asCollectionElement) ]
		ifNotNil: [ :element | element value: anObject asCollectionElement ].
	^ anObject
]

{ #category : #testing }
WeakValueDictionary >> includesKey: key [
	"Answer whether the receiver has a key equal to the argument and also that the value associated to this key was not garbage collected."

	^ (array at: (self scanFor: key))
		ifNil: [ false ]
		ifNotNil: [ :value | value value isNotNil ]
]

{ #category : #private }
WeakValueDictionary >> rehash [
	| newSelf |
	newSelf := self species new: self size.
	array do: [ :each | each value ifNotNil: [ :asso | newSelf noCheckAdd: each ] ].
	array := newSelf array
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
		(array at: eachIndex) value
			ifNotNil: [ :assocValue | aBlock value: assocValue enclosedElement ] ]
]
