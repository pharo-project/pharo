accessing
contentStream
	"Answer my contents as a text stream.
	Default is no conversion, since we don't know what the bytes mean."

	^self contentStreamFromEncoding: 'latin1'
