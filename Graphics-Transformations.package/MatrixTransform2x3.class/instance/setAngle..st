private
setAngle: angle
	"Set the raw rotation angle in the receiver"
	| rad s c |
	rad := angle degreesToRadians.
	s := rad sin.
	c := rad cos.
	self a11: c.
	self a12: s negated.
	self a21: s.
	self a22: c.