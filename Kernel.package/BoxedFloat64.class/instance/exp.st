mathematical functions
exp
	"Answer E raised to the receiver power.
	 Optional. See Object documentation whatIsAPrimitive." 

	| base fract correction delta div |
	<primitive: 59>

	"Taylor series"
	"check the special cases"
	self < 0.0 ifTrue: [^ (self negated exp) reciprocal].
	self = 0.0 ifTrue: [^ 1].
	self abs > MaxValLn ifTrue: [self error: 'exp overflow'].

	"get first approximation by raising e to integer power"
	base := E raisedToInteger: (self truncated).

	"now compute the correction with a short Taylor series"
	"fract will be 0..1, so correction will be 1..E"
	"in the worst case, convergance time is logarithmic with 1/Epsilon"
	fract := self fractionPart.
	fract = 0.0 ifTrue: [ ^ base ].  "no correction required"

	correction := 1.0 + fract.
	delta := fract * fract / 2.0.
	div := 2.0.
	[delta > Epsilon] whileTrue: [
		correction := correction + delta.
		div := div + 1.0.
		delta := delta * fract / div].
	correction := correction + delta.
	^ base * correction