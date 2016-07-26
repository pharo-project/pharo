actions
versionReaderForFileNamed: aString do: aBlock
	^ (self versionReaderForFileNamed: aString) ifNotNil: aBlock