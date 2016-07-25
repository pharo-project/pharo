accessing
searchBackwards: aBoolean
	searchBackwards := aBoolean.
	self updateFindStartIndex.
	self findPolicyChanged
