Extension { #name : 'FileSystem' }

{ #category : '*Deprecated14' }
FileSystem >> extensionDelimiter [

	self
		deprecated: 'Ask this to File.'
		in: #Pharo14
		transformWith: '`@rcv extensionDelimiter' -> 'File extensionDelimiter'.
	^ File extensionDelimiter
]
