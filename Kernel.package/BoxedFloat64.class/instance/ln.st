mathematical functions
ln
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	| expt n mant x div pow delta sum eps |
	<primitive: 58>

	"Taylor series"
	self <= 0.0 ifTrue: [^DomainError signal: 'ln is only defined for x > 0' from: 0].

	"get a rough estimate from binary exponent"
	expt := self exponent.
	n := Ln2 * expt.
	mant := self timesTwoPower: 0 - expt.

	"compute fine correction from mantinssa in Taylor series"
	"mant is in the range [0..2]"
	"we unroll the loop to avoid use of abs"
	x := mant - 1.0.
	div := 1.0.
	pow := delta := sum := x.
	x := x negated.  "x <= 0"
	eps := Epsilon * (n abs + 1.0).
	[delta > eps] whileTrue: [
		"pass one: delta is positive"
		div := div + 1.0.
		pow := pow * x.
		delta := pow / div.
		sum := sum + delta.
		"pass two: delta is negative"
		div := div + 1.0.
		pow := pow * x.
		delta := pow / div.
		sum := sum + delta].

	^ n + sum

	"2.718284 ln 1.0"