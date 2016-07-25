tests-valid extensions
testByteCanExtendByte

	self should: [ :builder | 
			builder layoutClass: ByteLayout ]
		extendWith: [ :builder | 
			builder layoutClass: ByteLayout ]