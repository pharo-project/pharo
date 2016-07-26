mathematical functions
sign
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0.
	Handle IEEE-754 negative-zero by reporting a sign of -1"

	self > 0 ifTrue: [^ 1].
	(self < 0 or: [((self at: 1) bitShift: -31) = 1]) ifTrue: [^ -1].
	^ 0