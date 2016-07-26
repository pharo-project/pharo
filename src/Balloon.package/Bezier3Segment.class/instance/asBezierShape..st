converting
asBezierShape: error
	"Demote a cubic bezier to a set of approximating quadratic beziers.
	Should convert to forward differencing someday"
	^(self asBezier2Points: error) asPointArray.