accessing
major: anObject

	self validateMajorVersion: anObject.
	major := anObject asInteger