prime numbers
primesUpTo: max do: aBlock
	"Compute aBlock with all prime integers up to the given integer."
	"Integer primesUpTo: 100"

	| limit flags prime k |
	limit := max asInteger - 1.
	"Fall back into #largePrimesUpTo:do: if we'd require more than 100k of memory; 
	the alternative will only requre 1/154th of the amount we need here and is almost as fast."
	limit > 25000 ifTrue:[^self largePrimesUpTo: max do: aBlock].
	flags := (Array new: limit) atAllPut: true.
	1 to: limit - 1 do: [:i |
		(flags at: i) ifTrue: [
			prime := i + 1.
			k := i + prime.
			[k <= limit] whileTrue: [
				flags at: k put: false.
				k := k + prime].
			aBlock value: prime]].
