prime numbers
largePrimesUpTo: max do: aBlock
	"Evaluate aBlock with all primes up to maxValue.
	The Algorithm is adapted from http://www.rsok.com/~jrm/printprimes.html
	It encodes prime numbers much more compactly than #primesUpTo: 
	38.5 integer per byte (2310 numbers per 60 byte) allow for some fun large primes.
	(all primes up to SmallInteger maxVal can be computed within ~27MB of memory;
	the regular #primesUpTo: would require 4 *GIGA*bytes).
	Note: The algorithm could be re-written to produce the first primes (which require
	the longest time to sieve) faster but only at the cost of clarity."

	| limit flags maskBitIndex bitIndex maskBit byteIndex index primesUpTo2310 indexLimit |
	limit := max asInteger - 1.
	indexLimit := max sqrt truncated + 1.
	"Create the array of flags."
	flags := ByteArray new: (limit + 2309) // 2310 * 60 + 60.
	flags atAllPut: 16rFF. "set all to true"

	"Compute the primes up to 2310"
	primesUpTo2310 := self primesUpTo: 2310.

	"Create a mapping from 2310 integers to 480 bits (60 byte)"
	maskBitIndex := Array new: 2310.
	bitIndex := -1. "for pre-increment"
	maskBitIndex at: 1 put: (bitIndex := bitIndex + 1).
	maskBitIndex at: 2 put: (bitIndex := bitIndex + 1).

	1 to: 5 do:[:i| aBlock value: (primesUpTo2310 at: i)].

	index := 6.
	2 to: 2309 do:[:n|
		[(primesUpTo2310 at: index) < n] 
			whileTrue:[index := index + 1].
		n = (primesUpTo2310 at: index) ifTrue:[
			maskBitIndex at: n+1 put: (bitIndex := bitIndex + 1).
		] ifFalse:[
			"if modulo any of the prime factors of 2310, then could not be prime"
			(n \\ 2 = 0 or:[n \\ 3 = 0 or:[n \\ 5 = 0 or:[n \\ 7 = 0 or:[n \\ 11 = 0]]]]) 
				ifTrue:[maskBitIndex at: n+1 put: 0]
				ifFalse:[maskBitIndex at: n+1 put: (bitIndex := bitIndex + 1)].
		].
	].

	"Now the real work begins...
	Start with 13 since multiples of 2,3,5,7,11 are handled by the storage method;
	increment by 2 for odd numbers only."
	13 to: limit by: 2 do:[:n|
		(maskBit := maskBitIndex at: (n \\ 2310 + 1)) = 0 ifFalse:["not a multiple of 2,3,5,7,11"
			byteIndex := n // 2310 * 60 + (maskBit-1 bitShift: -3) + 1.
			bitIndex := 1 bitShift: (maskBit bitAnd: 7).
			((flags at: byteIndex) bitAnd: bitIndex) = 0 ifFalse:["not marked -- n is prime"
				aBlock value: n.
				"Start with n*n since any integer < n has already been sieved 
				(e.g., any multiple of n with a number k < n has been cleared 
				when k was sieved); add 2 * i to avoid even numbers and
				mark all multiples of this prime. Note: n < indexLimit below
				limits running into LargeInts -- nothing more."
				n < indexLimit ifTrue:[
					index := n * n.
					(index bitAnd: 1) = 0 ifTrue:[index := index + n].
					[index <= limit] whileTrue:[
						(maskBit := maskBitIndex at: (index \\ 2310 + 1)) = 0 ifFalse:[
							byteIndex := (index // 2310 * 60) + (maskBit-1 bitShift: -3) + 1.
							maskBit := 255 - (1 bitShift: (maskBit bitAnd: 7)).
							flags at: byteIndex put: ((flags at: byteIndex) bitAnd: maskBit).
						].
						index := index + (2 * n)].
				].
			].
		].
	].
