accessing
write: anObject
	"Like <<, but returns the object putOn the receiver."

	anObject putOn: self.
	^ anObject.