initialize-release
readFrom: stream
	self readHeaderFrom: stream.
	self readEntityFrom: stream
	