tests - as string comma delimiter sequenceable
testAsCommaStringOne
	
	self nonEmpty1Element do: 
		[:each | 
		self assert: each asString =self nonEmpty1Element  asCommaString.
		self assert: each asString=self nonEmpty1Element  asCommaStringAnd.].

	