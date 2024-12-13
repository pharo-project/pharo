"
HashTableSizes is a helper class, used by hashedCollections to determine sizes for hash tables.

Public protocol is all class-side:

- `#goodSizeAtLeast:` anInteger  answers a ""good"" integer greater than or equal to the given integer.

An integer is not ""good"" as a hash table size if it is any of:
* Not prime
* Divides 256**k +- a, for small k and a
* Close to a power of two
* Close to dividing the hashMultiply constant

See Andres Valloud's hashing book, and Knuth TAOCP vol. 3.

This class caches a primary table of selected good primes within the positive SmallInteger range. When this table must be rebuilt, it uses an instance to compute the table. Primes are selected to keep the table fairly small, with approximately five entries per power of two. It is ordered, and is binary searched to find the closest good size >= the requested size.

This class caches a second table built from the first to support faster direct lookup of primes for requested sizes in the range of 0 to ""self maxSmallSize"".
"
Class {
	#name : #HashTableSizes,
	#superclass : #Object,
	#instVars : [
		'candidate',
		'goodPrimes',
		'primesToAvoid',
		'valuesNotToDivide'
	],
	#classInstVars : [
		'sizes',
		'smallSizesLookupTable'
	],
	#category : #'Collections-Unordered-Utilities'
}

{ #category : #public }
HashTableSizes class >> atLeast: lowerLimit [
	"Answer the next good size >= lowerlimit.
	If lowerLimit is larger than the largest known good prime,
	just make it odd."

	^ lowerLimit <= self maxSmallSize
		ifTrue: [
			self smallSizesLookupTable at:
				(lowerLimit <= 0
					ifTrue: [ 1 ]
					ifFalse: [ lowerLimit ceiling + 1 ]) ]
		ifFalse: [ self basicAtLeast: lowerLimit ]
]

{ #category : #private }
HashTableSizes class >> basicAtLeast: lowerLimit [
	"Binary search for the next good size >= lowerlimit.
	If lowerLimit is larger than the largest known good prime,
	just make it odd."

	| primes low mid high prime |
	primes := self sizes.
	low := 1.
	high := primes size.
	lowerLimit > (primes at: high)
		ifTrue:
			[ ^ lowerLimit even
				ifTrue: [ lowerLimit + 1 ]
				ifFalse: [ lowerLimit ] ].
	[ mid := (high - low) // 2 + low.
	prime := primes at: mid.
	prime < lowerLimit
		ifTrue: [ low := mid ]
		ifFalse: [ high := mid ].
	high - low <= 1
		ifTrue: [ ^ primes at: high ] ] repeat
]

{ #category : #'class initialization' }
HashTableSizes class >> initialize [
	"Throw away any previously-cached sizes, then compute and cache the sizes."

	sizes := nil.
	self sizes.

	smallSizesLookupTable := nil.
	self smallSizesLookupTable
]

{ #category : #private }
HashTableSizes class >> maxSmallSize [
	^ 255
]

{ #category : #private }
HashTableSizes class >> numValuesPerPower [
	"Answer the number of values that should be available in the cached table of primes
	for each power-of-two range. A larger number allows closer sizing for pre-sized collections,
	but results in a larger table that takes longer to search."

	^5 "Chosen so there will be fewer than 128 primes in the sizes table"
]

{ #category : #private }
HashTableSizes class >> sizes [
	sizes
		ifNil: [
			"Compute a sorted Array of known good table sizes that can be
			binary searched with #basicAtLeast:."
			sizes := self new computeSizes ].
	^ sizes
]

{ #category : #private }
HashTableSizes class >> smallSizesLookupTable [
	smallSizesLookupTable
		ifNil: [
			"Compute a lookup table of known good table sizes by caching the
			result of binary searching 'sizes' with #basicAtLeast: for a range
			of small sizes from 0 to #maxSmallSize."
			smallSizesLookupTable :=
				(0 to: self maxSmallSize) asArray collect: [ :each |
					self basicAtLeast: each ] ].
	^ smallSizesLookupTable
]

{ #category : #private }
HashTableSizes >> candidateIsGoodPrime [
	"Answer true if candidate will make a good hash table size.
	Some integers are rejected:
	* Non-primes
	* Primes which are close to dividing 1664525, the hashMultiply constant
	* Primes which divide 256**k +- a, for small k and a
	See Andres Valloud's hashing book, and Knuth TAOCP volume 3."

	candidate isPrime
		ifFalse: [ ^ false ].
	(primesToAvoid includes: candidate)
		ifTrue: [ ^ false ].
	candidate < 256
		ifTrue: [ ^ true ].	"Small primes cannot satisify divisibility constraints"
	^ valuesNotToDivide allSatisfy: [ :dividend | dividend \\ candidate ~~ 0 ]
]

{ #category : #private }
HashTableSizes >> computeSizes [
	"Answer an array of integers that make good hash table sizes.
	In each power of two, there are about five primes to choose from.
	Some primes are rejected:
	* Primes close to a power of two.
	* Primes which divide 256**k +- a, for small k and a
	* Primes which are close to dividing 1664525, the hashMultiply constant
	See Andres Valloud's hashing book, and Knuth TAOCP volume 3."

	| logInterval |
	logInterval := 0.5 / self numValuesPerPower.
	2 + logInterval to: 30 by: 2 * logInterval do:
			[ :exp |
			(self goodPrimeForExp: exp)
				ifNotNil:
					[ :prime |
					goodPrimes last ~~ prime
						ifTrue: [ goodPrimes add: prime ] ] ].
	^ goodPrimes asArray
]

{ #category : #private }
HashTableSizes >> firstCandidateForExp: exp [
	"Answer the smallest odd integer greater 2**exp."

	| n |
	n := (2 raisedTo: exp) rounded.
	^n odd
		ifTrue: [n]
		ifFalse: [n + 1]
]

{ #category : #private }
HashTableSizes >> goodPrimeForExp: exp [
	"Answer the next prime integer >= 2**exp that will make a good hash table size,
	Some primes are rejected:
	* Primes close to a power of two.
	* Primes which divide 256**k +- a, for small k and a
	* Primes which are close to dividing 1664525, the hashMultiply constant
	See Andres Valloud's hashing book, and Knuth TAOCP volume 3."

	| limit |

	candidate := self firstCandidateForExp: exp.
	limit := self limitForExp: exp.
	[ self candidateIsGoodPrime ]
		whileFalse:
			[ candidate := candidate + 2.
			candidate > limit
				ifTrue: [ ^ nil ] ].
	^ candidate
]

{ #category : #initialization }
HashTableSizes >> initialize [
	"Can't use any hashed collections, if sizes is being initialized might get infinite recursion"

	goodPrimes := OrderedCollection new.
	"Must contain a value less than any prime to avoid extra work in binary search"
	goodPrimes add: 0.
	valuesNotToDivide := OrderedCollection new.
	1 to: 8 do:
			[ :k |
			| n |
			n := 256 raisedToInteger: k.
			-32 to: 32 do: [ :a | valuesNotToDivide add: n + a ] ].
	primesToAvoid := self primeAlmostFactorsOf: 1 hashMultiply
]

{ #category : #private }
HashTableSizes >> limitForExp: exp [
	"Answer the largest integer that isn't too close to the next higher power of 2 than exp."

	| expLimit |
	expLimit := exp ceiling - (0.5 / self numValuesPerPower).
	^(2 raisedTo: expLimit) rounded
]

{ #category : #private }
HashTableSizes >> numValuesPerPower [
	"Answer the number of values that should be available in the cached table of primes
	for each power-of-two range."

	^self class numValuesPerPower
]

{ #category : #private }
HashTableSizes >> primeAlmostFactorsOf: anInteger [
	"Answer primes less than anInteger whose remainder when dividing anInteger is small"

	| factors |
	factors := OrderedCollection new.
	anInteger even ifTrue: [factors add: 2].
	3 to: anInteger // 2 + 2 by: 2 do: [:i |
		(i isPrime and: [| remainder |
						remainder := anInteger \\ i.
						remainder <= 1 or: [remainder = (i - 1)]])
			ifTrue: [factors add: i]].
	^factors asArray
]
