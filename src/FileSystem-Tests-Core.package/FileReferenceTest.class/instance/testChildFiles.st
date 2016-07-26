tests
testChildFiles
	| childFiles |
	filesystem createDirectory: '/beta'.
	filesystem createDirectory: '/gamma'.	
	(filesystem / 'delta')
		writeStreamDo: [ :stream | stream nextPutAll: '1' ].
	(filesystem / 'epsilon')
		writeStreamDo: [ :stream | stream nextPutAll: '2' ].
	childFiles := filesystem root files.
	self assert: childFiles size = 2.
	childFiles do: [ :each | 
		self assert: each class = FileReference.
		self assert: each isFile description: 'Collection should not contain references to directories.' ]