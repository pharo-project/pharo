Extension { #name : 'FileSystem' }

{ #category : '*Deprecated14' }
FileSystem >> extensionDelimiter [

	self deprecated: 'Ask this to File.' transformWith: '`@rcv extensionDelimiter' -> 'File extensionDelimiter'.
	^ File extensionDelimiter
]
