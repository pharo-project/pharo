tests-invalid extensions
testVariableCannotExtendWord

	self shouldnt: [ :builder | 
			builder layoutClass: WordLayout ]
		extendWith: [ :builder | 
			builder layoutClass: VariableLayout ]