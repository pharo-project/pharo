large integer arithmetic
isProbablyPrime: p
	"Answer true if p is prime with very high probability. Such a number is sometimes called an 'industrial grade prime'--a large number that is so extremely likely to be prime that it can assumed that it actually is prime for all practical purposes. This implementation uses the Rabin-Miller algorithm (Schneier, p. 159)."

	| iterations pMinusOne b m r a j z couldBePrime |
	iterations := 50.	"Note: The DSA spec requires >50 iterations; Schneier says 5 are enough (p. 260)"	"quick elimination: check for p divisible by a small prime"
	SmallPrimes
		ifNil: [ 
			"generate list of small primes > 2"
			SmallPrimes := Integer primesUpTo: 2000.
			SmallPrimes := SmallPrimes copyFrom: 2 to: SmallPrimes size ].
	SmallPrimes detect: [ :f | p \\ f = 0 ] ifFound: [ :factor | ^ p = factor ].
	pMinusOne := p - 1.
	b := self logOfLargestPowerOfTwoDividing: pMinusOne.
	m := pMinusOne bitShift: b negated.	"Assert: pMinusOne = m * (2 raisedTo: b) and m is odd"
	Transcript show: '      Prime test pass '.
	r := Random new.
	1 to: iterations do: [ :i | 
		Transcript
			show: i printString;
			space.
		a := (r next * 16rFFFFFF) truncated.
		j := 0.
		z := (a raisedTo: m modulo: p) normalize.
		couldBePrime := z = 1.
		[ couldBePrime ]
			whileFalse: [ 
				z = 1
					ifTrue: [ 
						Transcript
							show: 'failed!';
							cr.
						^ false ].	"not prime"
				z = pMinusOne
					ifTrue: [ couldBePrime := true ]
					ifFalse: [ 
						(j := j + 1) < b
							ifTrue: [ z := z * z \\ p ]
							ifFalse: [ 
								Transcript
									show: 'failed!';
									cr.
								^ false ] ] ] ].	"not prime"
	Transcript
		show: 'passed!';
		cr.
	^ true	"passed all tests; probably prime"