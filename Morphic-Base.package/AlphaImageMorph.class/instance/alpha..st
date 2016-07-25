accessing
alpha: anObject
	"Set the value of alpha"

	alpha := anObject.
	self
		cachedForm: nil;
		changed;
		changed: #alpha