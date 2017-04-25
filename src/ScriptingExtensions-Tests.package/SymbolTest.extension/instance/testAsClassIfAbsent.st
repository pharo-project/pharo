*ScriptingExtensions-Tests
testAsClassIfAbsent

	self 
		assert: (#Symbol asClassIfAbsent: [ self fail ])
		equals: Symbol.
		
	#+ asClassIfAbsent: [ ^ self "success" ].
	self fail.