tests - working
testPackageShouldFindWorkingCopy
	| packageReference workingCopy |
	packageReference := GoferPackageReference name: 'Gofer-Core'.
	workingCopy := packageReference workingCopy.
	self assert: workingCopy packageName = 'Gofer-Core'