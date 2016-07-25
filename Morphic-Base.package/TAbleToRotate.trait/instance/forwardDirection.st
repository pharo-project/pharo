t-rotating
forwardDirection
	"Return the receiver's forward direction (in eToy terms)"
	^self valueOfProperty: #forwardDirection ifAbsent:[0.0]