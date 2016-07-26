tests
testEmptyStrings
	| aStream zipStream |

	aStream := ByteArray new writeStream binary.
	zipStream := ZipWriteStream on: aStream.
	zipStream nextStringPut: ''. 
	zipStream nextStringPut: ''.
	zipStream close.
	aStream close.
	aStream := aStream contents readStream binary.
	zipStream := ZipReadStream on: aStream.
	self assert: zipStream nextString isEmpty. 
	self assert: zipStream nextString isEmpty.
	zipStream close.
	aStream close.