accessing
versionInfoForFileNamed: aString
	^ self infoCache 
		at: aString 
		ifAbsentPut: [ self versionReaderForFileNamed: aString do: [:r | r info]]