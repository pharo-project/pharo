private
candidateIsGoodPrime
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