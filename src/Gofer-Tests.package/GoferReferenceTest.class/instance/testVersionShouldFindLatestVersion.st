tests - reference
testVersionShouldFindLatestVersion
	| versionReference reference |
	versionReference := GoferVersionReference name: 'GoferFoo-lr.2'.
	reference := versionReference resolveWith: gofer.
	self assert: reference packageName = 'GoferFoo'.
	self assert: reference author = 'lr'.
	self assert: reference versionNumber = 2.
	self assert: reference branch isEmpty.
	self assert: reference repository = self monticelloRepository.
	
	versionReference := GoferVersionReference name: 'GoferFoo-lr.3'.
	self should: [ versionReference resolveWith: gofer ] raise: Error