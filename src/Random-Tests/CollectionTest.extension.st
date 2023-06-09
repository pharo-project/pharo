Extension { #name : #CollectionTest }

{ #category : #'*Random-Tests' }
CollectionTest >> testAtRandomWeighting [

	| random collection hits |
	collection := { 0. 1. 4. 3. 2. }.

	"Fixed randomness"
	random := Random seed: 42.

	"Collect results"
	hits := Bag new.
	1000 timesRepeat: [
		hits add: (collection atRandom: random weighting: [ :x | x ])
	].

	self assert: (hits occurrencesOf: 0) equals: 0. "exactly 0%"
	self assert: (hits occurrencesOf: 1) equals: 111. "approx 10%"
	self assert: (hits occurrencesOf: 2) equals: 180. "approx 20%"
	self assert: (hits occurrencesOf: 3) equals: 286. "approx 30%"
	self assert: (hits occurrencesOf: 4) equals: 423. "approx 40%"
]

{ #category : #'*Random-Tests' }
CollectionTest >> testAtRandomWeightingEmpty [

	| random collection |
	collection := { }.

	"Fixed randomness"
	random := Random seed: 42.

	self should: [  collection atRandom: random weighting: [ :x | x ] ] raise: Error.
]

{ #category : #'*Random-Tests' }
CollectionTest >> testAtRandomWeightingMultiple [

	| random collection hits |
	collection := { 0. 1. 0. 1. 0. 1. 5. 0. 1. 0. 1. 0. }.

	"Fixed randomness"
	random := Random seed: 42.

	"Collect results"
	hits := Bag new.
	1000 timesRepeat: [
		hits add: (collection atRandom: random weighting: [ :x | x ])
	].

	self assert: (hits occurrencesOf: 0) equals: 0. "exactly 0%"
	self assert: (hits occurrencesOf: 1) equals: 488. "approx 50%"
	self assert: (hits occurrencesOf: 5) equals: 512. "approx 50%"
]

{ #category : #'*Random-Tests' }
CollectionTest >> testAtRandomWeightingNegative [

	| random collection |
	collection := { -5 }.

	"Fixed randomness"
	random := Random seed: 42.

	self should: [  collection atRandom: random weighting: [ :x | x ] ] raise: Error.
]
