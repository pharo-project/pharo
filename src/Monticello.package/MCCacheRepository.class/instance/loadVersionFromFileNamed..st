private
loadVersionFromFileNamed: aString
	
	^ self versionReaderForFileNamed: aString do: [:r | r version]