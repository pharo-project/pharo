asserting
argumentNameForInstanceOf: aClass shouldBe: aString 
	
	self assert: aClass canonicalArgumentName equals: aString.