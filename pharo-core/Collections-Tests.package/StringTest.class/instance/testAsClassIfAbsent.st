tests - as class
testAsClassIfAbsent

	self 
		assert: ('String' asClassIfAbsent: [ self fail ])
		equals: String.
	
	'+' asClassIfAbsent: [ ^ self "success" ].
	self fail.