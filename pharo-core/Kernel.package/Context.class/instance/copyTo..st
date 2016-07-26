query
copyTo: aContext
	"Copy self and my sender chain down to, but not including, aContext.  End of copied chain will have nil sender."

	| copy |
	self == aContext ifTrue: [^ nil].
	copy := self copy.
	self sender ifNotNil: [
		copy privSender: (self sender copyTo: aContext)].
	^ copy