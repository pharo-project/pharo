public
loadWithNameLike: baseName
	self validate.
	self useNewChangeSetNamedLike: baseName during: [self basicLoad]