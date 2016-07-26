regression test
regressionCompress: aFile into: tempFile notifiying: progressBar stats: stats
	"Compress aFile into tempFile"
	| zip encoded buffer |
	aFile binary.
	aFile position: 0.
	tempFile binary.
	buffer := ByteArray new: 4096.
	zip := self on: (ByteArray new: 10000).
	encoded := zip encodedStream.
	[aFile atEnd] whileFalse:[
		progressBar current: aFile position.
		zip nextPutAll: (aFile nextInto: buffer).
		encoded position > 0 ifTrue:[
			tempFile nextPutAll: encoded contents.
			encoded position: 0]].
	zip close.
	tempFile nextPutAll: encoded contents.
	^true