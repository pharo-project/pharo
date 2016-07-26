printing
printOn: aStream
	"Display buffer sizes."

	aStream nextPutAll: self class name.
	inBuffer ifNotNil: [
		aStream nextPutAll: '[inbuf:',
		(inBuffer size / 1024) rounded asString, 'kb/outbuf:',
		(outBuffer size / 1024) rounded asString, 'kb]']