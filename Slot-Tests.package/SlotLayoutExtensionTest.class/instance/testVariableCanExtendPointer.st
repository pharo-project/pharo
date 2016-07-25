tests-valid extensions
testVariableCanExtendPointer

	self should: [ :builder | 
			builder layoutClass: FixedLayout ]
		extendWith: [ :builder | 
			builder layoutClass: VariableLayout ]