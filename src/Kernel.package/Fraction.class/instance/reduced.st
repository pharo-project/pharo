private
reduced

	| gcd numer denom |
	numerator = 0 ifTrue: [^0].
	gcd := numerator gcd: denominator.
	numer := numerator // gcd.
	denom := denominator // gcd.
	denom = 1 ifTrue: [^numer].
	^Fraction numerator: numer denominator: denom