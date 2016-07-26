tests-invalid extensions
testVariableCannotExtendByte

	self shouldnt: [ :builder | 
			builder layoutClass: ByteLayout ]
		extendWith: [ :builder | 
			builder layoutClass: VariableLayout ]