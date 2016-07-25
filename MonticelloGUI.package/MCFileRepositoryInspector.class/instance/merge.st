actions
merge
	super merge.
	self invalidatePackageAncestryCache.
	self refresh.
