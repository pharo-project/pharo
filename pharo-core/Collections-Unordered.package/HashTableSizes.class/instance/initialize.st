initialization
initialize
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