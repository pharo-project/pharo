convenience
encodedByteCountForString: string
	"Return the exact number of bytes it would take to encode string as a byte array"

	^ string 
		inject: 0 
		into: [ :sum :each |
			sum + (self encodedByteCountFor: each) ]