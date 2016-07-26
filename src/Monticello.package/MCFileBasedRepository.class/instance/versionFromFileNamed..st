accessing
versionFromFileNamed: aFileName
	| version |
	version := self cache
			 at: aFileName 
			 ifAbsent: [ self loadVersionFromFileNamed: aFileName ].
	self updateCachedVersionFromFileName: aFileName with: version.
	^ version