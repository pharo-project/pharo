writing
writeOn: stream
	| writeStream |
	writeStream := ZnBivalentWriteStream on: stream.
	writeStream nextPutAll: self version; space.
	self code printOn: writeStream.
	writeStream space; nextPutAll: self reason; nextPutAll: String crlf 