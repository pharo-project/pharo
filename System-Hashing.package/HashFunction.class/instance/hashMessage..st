accessing
hashMessage: aStringOrByteArray
	"MD5 new hashMessage: 'foo'"
	^ self hashStream: aStringOrByteArray asByteArray readStream