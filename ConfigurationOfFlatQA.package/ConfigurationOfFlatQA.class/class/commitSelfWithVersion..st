snapshotting
commitSelfWithVersion: versionNumber

	| version |

	version := self package mcPackage workingCopy
		newVersionWithMessage: 'Added version ', versionNumber
		in: self mainRepo.
	self mainRepo storeVersion: version.
		
	self commitToPublicRepos: version