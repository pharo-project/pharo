"
A LargeIdentitySet is an IdentitySet for large collections.
Rather than using linear probing, it takes advantage of the fact that identityHash (on a classic, 32bit VM) 
in Pharo only has 4096 unique values, using a bucket for each of those.
It will still work if hash range changes (ie buckets are chosen mod 4096), 
but the potential gain will be lower the more diverse the hash space is.

With linear probing you risk an array looking like this:
Index:	Hash:
	1	X
	2	X
	3	X
	4	4
	5	5
	6	4
	7	4
	8	7
	9	6
	10	X

While with buckets the same dataset looks:
Index:	Hash:
	1	X
	2	X
	3	X
	4	[4, 4, 4]
	5	5
	6	6
	7	7
	8	X
	9	X
	10	X

So includes: can generally be done faster (also sped up byusing a special primitive), 
and removal of objects does not have to do extensive cleanup if object was part of a chain.
"
Class {
	#name : #FLLargeIdentitySet,
	#superclass : #FLLargeIdentityHashedCollection,
	#instVars : [
		'array',
		'includesNil'
	],
	#category : 'Fuel-Collections'
}

{ #category : #adding }
FLLargeIdentitySet >> add: anObject [

	| hash |
	anObject ifNil: [ 
		includesNil ifFalse: [
			includesNil := true.
			tally := tally + 1 ].
		^anObject ].
	(array at: (hash := anObject largeIdentityHash \\ 4096 + 1))
		ifNil: [ 
			array at: hash put: (Array with: anObject).
			tallies at: hash put: 1.
			tally := tally + 1 ]
		ifNotNil: [ :list | 
			(list fuelPointsTo: anObject) ifFalse: [
				| newIndex |
				tally := tally + 1.
				list size < (newIndex := (tallies at: hash) + 1)
					ifFalse: [ list at: newIndex put: anObject ]
					ifTrue: [
						| newList |
						newList := Array new: list size * 2.
						newList
							replaceFrom: 1 
							to: list size
							with: list
							startingAt: 1;
							at: newIndex put: anObject.
						array at: hash put: newList ].
				tallies at: hash put: newIndex ] ].
	^anObject
]

{ #category : #adding }
FLLargeIdentitySet >> addIfNotPresent: anObject ifPresentDo: aBlock [

	| hash |
	anObject ifNil: [ 
		includesNil ifFalse: [
			includesNil := true.
			tally := tally + 1 ].
		^anObject ].
	(array at: (hash := anObject largeIdentityHash \\ 4096 + 1))
		ifNil: [ 
			array at: hash put: (Array with: anObject).
			tallies at: hash put: 1.
			tally := tally + 1 ]
		ifNotNil: [ :list | 
			(list fuelPointsTo: anObject) 
			ifTrue: [ aBlock value]
			ifFalse: [
				| newIndex |
				tally := tally + 1.
				list size < (newIndex := (tallies at: hash) + 1)
					ifFalse: [ list at: newIndex put: anObject ]
					ifTrue: [
						| newList |
						newList := Array new: list size * 2.
						newList
							replaceFrom: 1 
							to: list size
							with: list
							startingAt: 1;
							at: newIndex put: anObject.
						array at: hash put: newList ].
				tallies at: hash put: newIndex ] ].
	^anObject
]

{ #category : #enumerating }
FLLargeIdentitySet >> do: aBlock [

	tally = 0 ifTrue: [ ^self ].
	includesNil ifTrue: [ aBlock value: nil ].
	1 to: 4096 do: [ :arrayIndex |
		| subArray |
		subArray := array at: arrayIndex.
		1 to: (tallies at: arrayIndex) do: [ :index |
			aBlock value: (subArray at: index) ] ]
]

{ #category : #testing }
FLLargeIdentitySet >> includes: anObject [

	anObject ifNil: [ ^includesNil ].
	^(array at: (anObject largeIdentityHash \\ 4096 + 1))
		ifNil: [ false ]
		ifNotNil: [ :list | list fuelPointsTo: anObject ]
]

{ #category : #initialization }
FLLargeIdentitySet >> initialize [

	super initialize.
	array := Array new: 4096.
	includesNil := false
]

{ #category : #copying }
FLLargeIdentitySet >> postCopy [

	super postCopy.
	array := array copy.
	1 to: 4096 do: [ :index |
		(array at: index) ifNotNil: [ :list |
			array at: index put: list copy ] ]
]

{ #category : #removing }
FLLargeIdentitySet >> remove: anObject ifAbsent: aBlock [

	| list hash size |
	anObject ifNil: [
		includesNil ifTrue: [
			includesNil := false.
			tally := tally - 1.
			^anObject ].
		^aBlock value ].
	list := (array at: (hash := anObject largeIdentityHash \\ 4096 + 1)) ifNil: [ ^aBlock value ].
	(list fuelPointsTo: anObject) ifFalse: [ ^aBlock value ].
	1 to: (size := tallies at: hash) do: [ :index |
		(list at: index) == anObject 	ifTrue: [
			list
				at: index put: (list at: size);
				at: size put: nil.
			(size := size - 1) < (list size // 4) ifTrue: [
				| newList |
				newList := Array new: size * 2.
				newList
					replaceFrom: 1
					to: size
					with: list
					startingAt: 1.
				array at: hash put: newList ].
			tallies at: hash put: size.
			tally := tally - 1.
			^anObject ] ].
	^aBlock value
]
