tests - dictionary assocition access
testAssociationAtError

	| collection nonExistantKey |
	collection := self nonEmpty.
	nonExistantKey := self keyNotIn .

	self should: [collection associationAt: nonExistantKey] raise: Error. 

