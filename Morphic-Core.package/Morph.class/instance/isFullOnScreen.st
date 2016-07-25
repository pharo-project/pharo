testing
isFullOnScreen
	"Answer if the receiver is full contained in the owner visible  
	area."
	owner ifNil: [^ true].
	self visible ifFalse: [^ true].
	^ owner clearArea containsRect: self fullBounds