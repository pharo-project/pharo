tests - as class
testAsClassIfAbsent

	self 
		assert: (#Symbol asClassIfAbsent: [ self fail ])
		equals: Symbol.
		
	#+ asClassIfAbsent: [ ^ self "success" ].
	self fail.