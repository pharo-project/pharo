tests-invalid extensions
testPointerCannotExtendByte

	self shouldnt: [ :builder | 
			builder layoutClass: ByteLayout ]
		extendWith: [ :builder | 
			builder layoutClass: FixedLayout ]