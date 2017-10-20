tests - working
testVersionShouldFindWorkingCopy
	| versionReference workingCopy |
	versionReference := GoferVersionReference name: 'Gofer-Core-lr.18'.
	workingCopy := versionReference workingCopy.
	self assert: workingCopy packageName = 'Gofer-Core'