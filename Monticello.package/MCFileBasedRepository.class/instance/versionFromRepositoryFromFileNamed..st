accessing
versionFromRepositoryFromFileNamed: aFileName
	| version |
	version := self cache
			 at: aFileName 
			 ifAbsent: [ self loadNotCachedVersionFromFileNamed: aFileName ].
	self updateCachedVersionFromFileName: aFileName with: version.
	^ version