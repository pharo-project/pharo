tests - reference
testPackageShouldFindLatestVersion
	| packageReference reference |
	packageReference := GoferPackageReference name: 'GoferFoo'.
	reference := packageReference resolveWith: gofer.
	self assert: reference packageName = 'GoferFoo'.
	self assert: reference author = 'lr'.
	self assert: reference branch isEmpty.
	self assert: reference versionNumber = 4.
	self assert: reference repository = self monticelloRepository