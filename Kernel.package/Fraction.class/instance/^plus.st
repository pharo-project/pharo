arithmetic
+ aNumber 
	"Answer the sum of the receiver and aNumber."
	| n d d1 d2 |
	aNumber isInteger ifTrue:
		[^Fraction numerator: numerator + (denominator * aNumber) denominator: denominator].
	aNumber isFraction ifTrue: 
		[d := denominator gcd: aNumber denominator.
		n := numerator * (d1 := aNumber denominator // d) + (aNumber numerator * (d2 := denominator // d)).
		d1 := d1 * d2.
		n := n // (d2 := n gcd: d).
		(d := d1 * (d // d2)) = 1 ifTrue: [^ n].
		^ Fraction numerator: n denominator: d].
	^ aNumber adaptToFraction: self andSend: #+