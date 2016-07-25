private
loadNotCachedVersionFromFileNamed: aString
	
	^ self versionReaderForFileNamed: aString do: [:r | r version]