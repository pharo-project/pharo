initialize-release
readHeaderFrom: stream
	self headers: (ZnHeaders readFrom: stream)
	