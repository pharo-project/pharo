actions
snapshot
	| version |
	version := utilWorkingCopy newVersionWithMessage: 'Test snapshot.' in: utilRepo.
	currentSnapshot := version snapshot.
	utilRepo storeVersion: version.