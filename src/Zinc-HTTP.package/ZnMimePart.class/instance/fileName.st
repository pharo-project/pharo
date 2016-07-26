accessing
fileName
	"Pathnames are often silenty encoded using UTF-8,
	this is a no-op for ASCII, but will fail on Latin-1 and others"

	^ (self detectContentDispositionValue: 'filename')
		ifNotNil: [ :encodedFileName | encodedFileName asByteArray utf8Decoded ]