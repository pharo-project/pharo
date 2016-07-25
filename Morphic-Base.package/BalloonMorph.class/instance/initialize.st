initialization
initialize
	"initialize the state of the receiver"
	
	balloonColor := self class balloonColor.
	super initialize.
	self beSmoothCurve.
	offsetFromTarget := 0@0.