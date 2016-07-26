mathematical functions
sin
	"Answer the sine of the receiver taken as an angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	| sum delta self2 i |
	<primitive: 556>

	"Taylor series"
	"normalize to the range [0..Pi/2]"
	self < 0.0 ifTrue: [^ (0.0 - ((0.0 - self) sin))].
	self > Twopi ifTrue: [^ (self \\ Twopi) sin].
	self > Pi ifTrue: [^ (0.0 - (self - Pi) sin)].
	self > Halfpi ifTrue: [^ (Pi - self) sin].

	"unroll loop to avoid use of abs"
	sum := delta := self.
	self2 := 0.0 - (self * self).
	i := 2.0.
	[delta > Epsilon] whileTrue: [
		"once"
		delta := (delta * self2) / (i * (i + 1.0)).
		i := i + 2.0.
		sum := sum + delta.
		"twice"
		delta := (delta * self2) / (i * (i + 1.0)).
		i := i + 2.0.
		sum := sum + delta].
	^ sum