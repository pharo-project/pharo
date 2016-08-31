tests
testOneTwo
	| stub |
	stub := self stubClass one: #one two: #two.
	self
		assert: stub one = #one;
		assert: stub two = #two;
		assert: (stub one: #three two: #four) == stub;
		assert: stub one = #three;
		assert: stub two = #four;
		assert: (stub oneTwo: #five) == stub;
		assert: stub one = #three;
		assert: stub two = #five