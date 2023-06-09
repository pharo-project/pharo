"
PluggableSets allow the redefinition of hashing and equality by clients. 
This is in particular useful if the clients know about specific properties of 
the objects stored in the set which in turn can heavily improve the performance 
of sets and dictionaries.

Note: As of Pharo 1.1#11284, using normal `Dictionary` is actually faster as the bench below shows... ;-)

Instance variables:
	hashBlock	<BlockContext>	A one argument block used for hashing the elements.
	equalBlock	<BlockContext>	A two argument block used for comparing the elements.

### Example
Adding 1000 integer points in the range (0@0) to: (100@100) to a set.
```
	| rnd set max pt |
	set := Set new: 1000.
	rnd := Random new.
	max := 100.
	Time millisecondsToRun:[
		1 to: 1000 do:[:i|
			pt := (rnd next * max) truncated @ (rnd next * max) truncated.
			set add: pt.
		].
	].
```
The above is way slow since the default hashing function of points leads 
to an awful lot of collisions in the set. And now the same, with a somewhat different hash function:
```
	| rnd set max pt |
	set := PluggableSet new: 1000.
	set hashBlock:[:item| (item x bitShift: 16) + item y].
	rnd := Random new.
	max := 100.
	Time millisecondsToRun:[
		1 to: 1000 do:[:i|
			pt := (rnd next * max) truncated @ (rnd next * max) truncated.
			set add: pt.
		].
	].
```
"
Class {
	#name : #PluggableSet,
	#superclass : #Set,
	#instVars : [
		'hashBlock',
		'equalBlock'
	],
	#category : #'Collections-Unordered-Sets'
}

{ #category : #accessing }
PluggableSet class >> integerSet [
	^self new hashBlock: [:integer | integer hash \\ 1064164 * 1009]
]

{ #category : #copying }
PluggableSet >> copyEmpty [
	^super copyEmpty
		hashBlock: hashBlock;
		equalBlock: equalBlock
]

{ #category : #accessing }
PluggableSet >> equalBlock [
	"Return the block used for comparing the elements in the receiver."
	^equalBlock
]

{ #category : #accessing }
PluggableSet >> equalBlock: aBlock [
	"Set a new equality block. The block must accept two arguments and return true if the argumets are considered equal, false otherwise"
	equalBlock := aBlock
]

{ #category : #accessing }
PluggableSet >> hashBlock [
	"Return the block used for hashing the elements in the receiver."
	^hashBlock
]

{ #category : #accessing }
PluggableSet >> hashBlock: aBlock [
	"Set a new hash block. The block must accept one argument and return the hash value of the given argument."
	hashBlock := aBlock
]

{ #category : #private }
PluggableSet >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or raise an error if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| index start |
	index := start := (hashBlock ifNil: [anObject hash] ifNotNil: [ hashBlock value: anObject]) \\ array size + 1.
	[
		| element |
		((element := array at: index) == nil or: [
			equalBlock ifNil: [element enclosedElement = anObject] ifNotNil: [
				equalBlock value: element enclosedElement value: anObject ]])
			ifTrue: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace
]

{ #category : #private }
PluggableSet >> scanForEmptySlotFor: aKey [
	"Scan the key array for the first slot containing an empty slot (indicated by a nil). Answer the index of that slot. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| index start |
	index := start := (hashBlock
		ifNil: [ aKey hash ]
		ifNotNil: [ hashBlock value: aKey ]) \\ array size + 1.
	[
		(array at: index) ifNil: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace
]
