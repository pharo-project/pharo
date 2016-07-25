testing-empty
testEmptyStringAtWordBoundary
	| matcher |
	matcher := self matcherClass forString: '\b'.
	self
		assert: (matcher copy: 'foo bar' replacingMatchesWith: '*')
			= '*foo* *bar*'
		description: 'An empty string at a word boundary'