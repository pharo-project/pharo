mathematical functions
arcTan
	"Answer the angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	| theta eps step sinTheta cosTheta |
	<primitive: 57>

	"Newton-Raphson"
	self < 0.0 ifTrue: [ ^ 0.0 - (0.0 - self) arcTan ].

	"first guess"
	theta := (self * Halfpi) / (self + 1.0).

	"iterate"
	eps := Halfpi * Epsilon.
	step := theta.
	[(step * step) > eps] whileTrue: [
		sinTheta := theta sin.
		cosTheta := theta cos.
		step := (sinTheta * cosTheta) - (self * cosTheta * cosTheta).
		theta := theta - step].
	^ theta