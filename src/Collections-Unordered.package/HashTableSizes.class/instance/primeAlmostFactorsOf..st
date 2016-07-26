private
primeAlmostFactorsOf: anInteger
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
