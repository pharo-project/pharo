tests-valid extensions
testCompiledMethodCanBeExtended

	self should: [ :builder | 
			builder layoutClass: CompiledMethodLayout ]
		extendWith: [ :builder | 
			builder layoutClass: CompiledMethodLayout ]