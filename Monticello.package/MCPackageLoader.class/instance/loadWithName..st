public
loadWithName: baseName
	self validate.
	self useChangeSetNamed: baseName during: [self basicLoad]