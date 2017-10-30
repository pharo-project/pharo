tests - working
testResolvedShouldFindWorkingCopy
	| versionReference workingCopy |
	versionReference := GoferResolvedReference name: 'Gofer-Core-lr.18' repository: self monticelloRepository.
	workingCopy := versionReference workingCopy.
	self assert: workingCopy packageName = 'Gofer-Core'