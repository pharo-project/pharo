interface
parseFileNamesFromStream: aNewLineDelimitedString
	^ aNewLineDelimitedString 
		ifNil: [ ^ OrderedCollection new ]
		ifNotNil: [ aNewLineDelimitedString substrings: String crlf ]