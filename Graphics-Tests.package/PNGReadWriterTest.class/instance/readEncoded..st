helpers
readEncoded: bytes
	"Answer a ReadStream on the file named by fileName, if possible; else a ReadStream on bytes"

	fileName ifNil:[^ bytes readStream ].
	^(FileStream oldFileOrNoneNamed: fileName) ifNil: [ 
		Transcript nextPutAll: 'can''t open ', fileName; cr.
		bytes readStream ].
