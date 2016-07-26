tests
testChildDirectories
	| childDirectories |
	filesystem createDirectory: '/beta'.
	filesystem createDirectory: '/gamma'.	
	(filesystem / 'delta')
		writeStreamDo: [ :stream | stream nextPutAll: '1' ].
	(filesystem / 'epsilon')
		writeStreamDo: [ :stream | stream nextPutAll: '2' ].
	childDirectories := filesystem root directories.
	self assert: childDirectories size = 2.
	childDirectories do: [ :each | 
		self assert: each class = FileReference.
		self assert: each isDirectory description: 'Collection should not contain references to files.' ]