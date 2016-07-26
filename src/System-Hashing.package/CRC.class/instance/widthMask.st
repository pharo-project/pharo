private-bit-manipulation
widthMask 	
	"bit mask (all ones)"
	^ widthMask ifNil: [ widthMask := (2 raisedTo: width) - 1 ]