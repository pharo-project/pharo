development support
updatePackageRepositories: repositorySpecs

	| resolvedPackageRef |
	Transcript cr; show: '  Looking up version -> ', self file.
	resolvedPackageRef := self loader resolveSpec: self from: repositorySpecs.
	Transcript cr; show: 'Update repositoryGroup -> ', resolvedPackageRef name, ' ' , resolvedPackageRef repository description.
	resolvedPackageRef version workingCopy repositoryGroup addRepository: resolvedPackageRef repository