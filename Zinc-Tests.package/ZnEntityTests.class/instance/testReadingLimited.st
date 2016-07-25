testing
testReadingLimited
	| contents |
	contents := String new: 1025 withAll: $X.
	ZnMaximumEntitySize value: 1024 during: [
		self should: [
			ZnEntity 
				readFrom: contents asByteArray readStream 
				usingType: ZnMimeType textPlain 
				andLength: contents size ]
			raise: ZnEntityTooLarge.
		self should: [
			ZnEntity 
				readFrom: contents asByteArray readStream 
				usingType: ZnMimeType textPlain 
				andLength: nil ]
			raise: ZnEntityTooLarge ] 