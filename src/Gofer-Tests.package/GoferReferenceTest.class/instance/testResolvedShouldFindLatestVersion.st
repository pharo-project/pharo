tests - reference
testResolvedShouldFindLatestVersion
	| versionReference reference |
	versionReference := GoferResolvedReference name: 'GoferFoo-lr.2' repository: self monticelloRepository.
	reference := versionReference resolveWith: gofer.
	self assert: reference packageName = 'GoferFoo'.
	self assert: reference author = 'lr'.
	self assert: reference branch isEmpty.
	self assert: reference versionNumber = 2.
	self assert: reference repository = self monticelloRepository