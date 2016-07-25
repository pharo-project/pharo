accessing
enabled: anObject
	"Set the value of enabled"

	enabled := anObject.
	self
		cachedForm: nil;
		changed