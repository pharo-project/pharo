*Pharo30Bootstrap
written: aCGOLoadInstructionsFileRef sourceRepository: sourceRepositoryUrl
	| mcVersions |
	mcVersions := (STON fromStream: aCGOLoadInstructionsFileRef readStream) 
		mcVersionsFromRepositories: { 	MCRepository fromUrl: sourceRepositoryUrl }.
	^ self ficusEnvironmentFromMcVersions: mcVersions flattened 