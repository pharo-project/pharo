development support
copySpecTo: aRepositorySpec
	"Copy current mcz file to the repository named in aRepositorySpec"

	self loader copySpec: self from: self repositorySpecs to: aRepositorySpec createRepository