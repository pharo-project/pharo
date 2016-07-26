private
goodPrimeForExp: exp
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