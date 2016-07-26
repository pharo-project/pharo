asserting
assertSnapshot: actual matches: expected
	| diff |
	diff := actual patchRelativeToBase: expected.
	self assert: diff isEmpty
