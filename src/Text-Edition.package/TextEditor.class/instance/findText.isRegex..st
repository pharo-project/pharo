accessing
findText: aString isRegex: aBoolean
	self findReplaceService findText: aString isRegex: aBoolean.
	self findReplaceService changed: #newFinding
