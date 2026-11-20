Extension { #name : 'Path' }

{ #category : '*Deprecated14-Kernel' }
Path class >> extensionDelimiter [

	self
		deprecated: 'Ask this to File.'
		in: #Pharo14
		transformWith: '`@rcv extensionDelimiter' -> 'File extensionDelimiter'.
	^ File extensionDelimiter
]
