"
I share behavior for special HashedCollections that are optimized for including a large number of elements.
"
Class {
	#name : #FLLargeIdentityHashedCollection,
	#superclass : #Collection,
	#instVars : [
		'tally',
		'tallies'
	],
	#classVars : [
		'PermutationMap'
	],
	#category : 'Fuel-Core-Collections'
}

{ #category : #'initialize-release' }
FLLargeIdentityHashedCollection class >> initialize [

	| rng |
	rng := Random seed: 664399324.
	PermutationMap := (0 to: 4095) asArray shuffleBy: rng
]

{ #category : #comparing }
FLLargeIdentityHashedCollection class >> permuteHash: anInteger [
	"Return an integer between 1 and 4096 when the argument is between 1 and 4096."

	^PermutationMap at: anInteger
]

{ #category : #initialization }
FLLargeIdentityHashedCollection >> initialize [

	tally := 0.
	tallies := Array new: 4096 withAll: 0
]

{ #category : #copying }
FLLargeIdentityHashedCollection >> postCopy [

	tallies := tallies copy

]

{ #category : #accessing }
FLLargeIdentityHashedCollection >> size [

	^tally
]
