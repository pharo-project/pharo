testing-empty
testEmptyStringAtEndOfWord
	| matcher |
	matcher := self matcherClass forString: '\>'.
	self
		assert: (matcher copy: 'foo bar' replacingMatchesWith: '*')
			= 'foo* bar*'
		description: 'An empty string at the end of a word'