tests-invalid extensions
testByteCannotExtendWord

	self shouldnt: [ :builder | 
			builder layoutClass: WordLayout ]
		extendWith: [ :builder | 
			builder layoutClass: ByteLayout ]