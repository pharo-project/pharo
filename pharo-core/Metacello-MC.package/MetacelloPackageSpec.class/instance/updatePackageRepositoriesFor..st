development support
updatePackageRepositoriesFor: aVersionSpec
	"Don't update the repository unless the package is loaded in the image"
	
	self workingCopy == nil ifTrue: [ ^self ].
	self updatePackageRepositories: self repositorySpecs, aVersionSpec repositorySpecs.