asserting
argumentNameFor: anObject shouldBe: aString 

	self assert: anObject class canonicalArgumentName equals: aString.