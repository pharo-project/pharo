converting
decode: string
	"Decode a Base64 encoded string and return the resulting byte array"
	
	^ ByteArray 
		new: (self byteCountFor: string)
		streamContents: [ :byteStream | 
			self decode: string readStream to: byteStream ]