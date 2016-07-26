operations
moveTo: aReference
	
	| result |
	result := self fileSystem 
		rename: self path
		to: aReference resolve path.
	result ifNotNil: [
		self setFileSystem: filesystem path: aReference path ].		