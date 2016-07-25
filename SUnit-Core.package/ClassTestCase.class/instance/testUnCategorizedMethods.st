tests
testUnCategorizedMethods
	| uncategorizedMethods  |
	uncategorizedMethods := self targetClass selectorsInProtocol: Protocol unclassified.
	self 
		assert: uncategorizedMethods isEmpty
		description: uncategorizedMethods asString	