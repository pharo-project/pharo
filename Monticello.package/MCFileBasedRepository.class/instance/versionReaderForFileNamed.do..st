actions
versionReaderForFileNamed: aString do: aBlock
	^ self
		readStreamForFileNamed: aString
		do: [:s |
			(MCVersionReader readerClassForFileNamed: aString) ifNotNil:
				[:class | aBlock value: (class on: s fileName: aString)]]
