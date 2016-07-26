running
setUp
	self tearDown.
	ZnFileSystemUtils
		newFileNamed: 'small.html' 
		do: [ :stream |
			stream nextPutAll: self smallHtml ].
	ZnFileSystemUtils
		newFileNamed: 'large.html' 
		do: [ :stream |
			stream nextPutAll: self largeHtml ].
	ZnFileSystemUtils
		newFileNamed: 'wide.html' 
		do: [ :stream |
			stream nextPutAll: self wideHtml ].