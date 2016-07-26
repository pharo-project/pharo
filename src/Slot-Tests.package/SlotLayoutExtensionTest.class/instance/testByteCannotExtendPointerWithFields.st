tests-invalid extensions
testByteCannotExtendPointerWithFields

	self shouldnt: [ :builder | 
			builder 
				layoutClass: FixedLayout;
				slots: { #a }. ]
		extendWith: [ :builder | 
			builder layoutClass: ByteLayout ]