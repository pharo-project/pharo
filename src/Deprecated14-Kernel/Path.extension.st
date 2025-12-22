Extension { #name : 'Path' }

{ #category : '*Deprecated14-Kernel' }
Path class >> extensionDelimiter [

	self deprecated: 'Ask this to File.' transformWith: '`@rcv extensionDelimiter' -> 'File extensionDelimiter'.
	^ File extensionDelimiter
]
