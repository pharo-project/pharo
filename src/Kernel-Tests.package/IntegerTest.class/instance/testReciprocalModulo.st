tests - arithmetic
testReciprocalModulo
	1 to: 512 do: [:a |
		a + 1 to: 512 do: [:b |
			| c |
			(a gcd: b) = 1
				ifTrue:
					[c := a reciprocalModulo: b.
					self assert: (a * c) \\ b = 1]
				ifFalse: [self should: [ a reciprocalModulo: b ] raise: Error]]].