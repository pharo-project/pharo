loading
loadVersionInfoFromFileNamed: aString
	^ self versionReaderForFileNamed: aString do: [:r | r info]
	