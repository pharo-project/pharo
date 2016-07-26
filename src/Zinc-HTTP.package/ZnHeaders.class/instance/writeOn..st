writing
writeOn: stream
	| writeStream |
	writeStream := ZnBivalentWriteStream on: stream.
	self headersDo: [ :key :value |
		writeStream nextPutAll: key; nextPut: $:; space; nextPutAll: value; nextPutAll: String crlf ]