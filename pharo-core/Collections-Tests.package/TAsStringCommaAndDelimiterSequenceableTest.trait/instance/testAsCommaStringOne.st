tests - comma and delimiter
testAsCommaStringOne
	
	"self assert: self oneItemCol asCommaString = '1'.
	self assert: self oneItemCol asCommaStringAnd = '1'."

	self assert: self nonEmpty1Element  asCommaString = (self nonEmpty1Element first asString).
	self assert: self nonEmpty1Element  asCommaStringAnd = (self nonEmpty1Element first asString).
	