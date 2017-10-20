tests - repositories
testDirectoryRepository
	gofer directory: FileSystem disk workingDirectory fullName.
	self assert: gofer repositories: (Array with: FileSystem disk workingDirectory fullName).
	self assert: (gofer repositories first isKindOf: MCDirectoryRepository)