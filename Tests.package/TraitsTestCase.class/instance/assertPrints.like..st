utility
assertPrints: aString like: anotherString 
	self assert: (aString copyWithout: $ )
		= (anotherString copyWithout: $ )