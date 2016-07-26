private
cacheDirectoryFor: projectPath
	| cacheDirectory projectDirectory |
	
	cacheDirectory := self cacheDirectory.
	projectDirectory := MetacelloPlatform current directoryFromPath: projectPath relativeTo: cacheDirectory.
	MetacelloPlatform current ensureDirectoryExists: projectDirectory.
	^ projectDirectory