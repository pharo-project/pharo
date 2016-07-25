testing
testPostMultipart
	| client filename |
	ZnFileSystemUtils deleteIfExists: 'testPostMultiPart.txt'.
	filename := ZnFileSystemUtils fullNameFor: 'testPostMultiPart.txt'.
	ZnFileSystemUtils newFileNamed: filename do: [ :stream | stream nextPutAll: '0123456789' ].
	self withServerDo: [ :server |
		(client := ZnClient new)
			url: server localUrl;
			addPath: 'echo';
			addPart: (ZnMimePart fieldName: 'username' value: 'john');
			addPart: (ZnMimePart fieldName: 'foo' entity: (ZnEntity text: '123'));
			addPart: (ZnMimePart fieldName: 'file' fileNamed: filename);
			post.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'multipart') ].
	ZnFileSystemUtils deleteIfExists: 'testPostMultiPart.txt'.
