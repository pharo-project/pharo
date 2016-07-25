tests-valid extensions
testVariableCanExtendVariable

	self should: [ :builder | 
			builder layoutClass: VariableLayout ]
		extendWith: [ :builder | 
			builder layoutClass: VariableLayout ]