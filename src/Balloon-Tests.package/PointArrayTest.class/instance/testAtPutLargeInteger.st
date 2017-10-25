tests
testAtPutLargeInteger

	self should:  [pointArray at: 2 put: 123456789012345678901234567890@987654323456787654378989] raise: Error.	