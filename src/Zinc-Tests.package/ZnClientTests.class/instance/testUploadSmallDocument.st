testing
testUploadSmallDocument
	self withServerDo: [ :server | | client contents path |
		path := ZnFileSystemUtils fullNameFor: 'small.txt'.
		contents := String streamContents: [ :stream |
			stream print: DateAndTime now; space; print: 9999 atRandom ].
		ZnFileSystemUtils deleteIfExists: 'small.txt'.
		ZnFileSystemUtils newFileNamed: path do: [ :stream | stream nextPutAll: contents ].
		(client := ZnClient new)
			url: server localUrl;
			addPath: 'echo';
			uploadEntityFrom: path.
		self assert: client request entity contentType = ZnMimeType textPlain.
		client contentType: ZnMimeType textPlain. "Not needed, just test the code path"
		self assert: client request entity contentType = ZnMimeType textPlain.
		client post.
		self assert: client isSuccess.
		self assert: client entity contentType = ZnMimeType textPlain.
		self assert: (client contents includesSubstring: contents).
		client close.
		ZnFileSystemUtils deleteIfExists: 'small.txt' ]