"
This class represents an identity dictionary with weak keys.
"
Class {
	#name : #WeakIdentityKeyDictionary,
	#superclass : #WeakKeyDictionary,
	#category : #'Collections-Weak-Dictionaries'
}

{ #category : #private }
WeakIdentityKeyDictionary >> compare: object1 to: object2 [
	^ object1 == object2
]

{ #category : #private }
WeakIdentityKeyDictionary >> scanForEmptySlotFor: aKey [
	"Scan the key array for the first slot containing an empty slot (indicated by a nil). Answer the index of that slot. This method will be overridden in various subclasses that have different interpretations for matching elements."
	
	| index start |
	index := start := aKey identityHash \\ array size + 1.
	[ 
		(array at: index) ifNil: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace
]

{ #category : #private }
WeakIdentityKeyDictionary >> startIndexFor: anObject [
	"Return the index at which the scan for anObject should start."
	
	^(anObject identityHash \\ array size) + 1
]

{ #category : #enumeration }
WeakIdentityKeyDictionary >> valuesDo: aBlock [ 
	"See comments in Dictionary>>valuesDo:.  The code keeps the key so it's
	not collected during the evaluation of aBlock"

	tally = 0 ifTrue: [ ^self ].
	1 to: array size do:
		[ :eachIndex | 
			| eachAssociation eachKey |
			eachAssociation := array at: eachIndex.
			nil == eachAssociation ifFalse:
				[
					eachKey := eachAssociation key.
					nil == eachKey ifFalse: [ aBlock value: eachAssociation value ]
				]
		]
]
