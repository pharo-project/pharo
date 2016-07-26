accessing
doubleHashMessage: aStringOrByteArray
	"SHA1 new doubleHashMessage: 'foo'"
	^ self doubleHashStream: aStringOrByteArray asByteArray readStream