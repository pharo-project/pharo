private
computeSizes
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