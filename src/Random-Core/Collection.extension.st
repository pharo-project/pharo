Extension { #name : 'Collection' }

{ #category : '*Random-Core' }
Collection >> atRandom [
	"Answer a random element of the receiver.  
	
	Examples:
	10 atRandom
	#('one' 'or' 'the' 'other') atRandom
	'Just pick one of these letters at random' atRandom
	(Set withAll: #(3 7 4 9 21)) atRandom		

	Implementation note: It uses a shared random number generator.  
	If you use this a lot, define your own instance of Random and use #atRandom:."

	^ self atRandom: SharedRandom globalGenerator.


]

{ #category : '*Random-Core' }
Collection >> atRandom: aGenerator [
	"Answer a random element of the receiver. Uses aGenerator which
    should be kept by the user in a variable and used every time. Use
    this instead of #atRandom for better uniformity of random numbers because
	only you use the generator. Raises an error if the receiver has no elements."

	| rand |
	self emptyCheck.
	rand := aGenerator nextInteger: self size.
	self withIndexDo: [:each :index | index = rand ifTrue: [ ^ each ]]
]

{ #category : '*Random-Core' }
Collection >> atRandom: aRandom weighting: aBlock [

	"Retrieve an element with a probability related to the weighting block.
	The block takes an element as argument and provides a positive number as result (the retative weight).
	An implicit normalization is performed on all weights.

	Multiple occurences of equal elements multiply the probability to be choosen.

	Warning: empty collections and negative weights will signal errors."

	"({'1'. '2'. '3'. '4'} atRandom: (Random seed: -42) weighting: [ :each | each asInteger ]) >>> '4'"
	"({'1'. '1'. '2'. '1'. '1'} atRandom: (Random seed: -42) weighting: [ :each | each asInteger ]) >>> '1'"

	| sum |
	self emptyCheck.

	sum := self sum: aBlock.
	sum := sum * aRandom next.

	self do: [ :each |
		| weight |
		weight := aBlock value: each.
		weight < 0 ifTrue: [ self error: 'Unexpected negative weight' ].
		sum := sum - weight.
		sum <= 0 ifTrue: [ ^ each ] ].
	self error: 'This should not happen'
]

{ #category : '*Random-Core' }
Collection >> atRandomWeighting: aBlock [

	^ self atRandom: SharedRandom globalGenerator weighting: aBlock
]
