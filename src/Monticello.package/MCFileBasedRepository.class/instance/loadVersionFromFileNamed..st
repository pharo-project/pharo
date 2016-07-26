private
loadVersionFromFileNamed: aString

	(MCCacheRepository uniqueInstance includesFileNamed: aString)
		ifTrue: [ ^ MCCacheRepository uniqueInstance loadVersionFromFileNamed: aString].
	
	^ self versionReaderForFileNamed: aString do: [:r | r version]