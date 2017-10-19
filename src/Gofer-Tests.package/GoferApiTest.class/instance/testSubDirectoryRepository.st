tests - repositories
testSubDirectoryRepository
	Smalltalk globals
		at: #MCSubDirectoryRepository
		ifPresent: [ :subDirectoryRepositoryClass | |path|
			path := (FileSystem disk workingDirectory / '*') fullName.
			gofer directory: path.
			self assert: gofer repositories: (Array with: path).
			self assert: (gofer repositories first isKindOf: subDirectoryRepositoryClass) ]