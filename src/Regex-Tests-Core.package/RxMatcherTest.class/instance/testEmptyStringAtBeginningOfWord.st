testing-empty
testEmptyStringAtBeginningOfWord
	| matcher |
	matcher := self matcherClass forString: '\<'.
	self
		assert: (matcher copy: 'foo bar' replacingMatchesWith: '*')
			= '*foo *bar'
		description: 'An empty string at the beginning of a word'