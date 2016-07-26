initialize-release
finish
	self writeChunkSize: 0.
	self crlf.
	extraHeaders ifNotNil: [ extraHeaders writeOn: stream ]