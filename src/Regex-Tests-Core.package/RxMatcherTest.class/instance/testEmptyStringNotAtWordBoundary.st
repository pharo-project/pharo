testing-empty
testEmptyStringNotAtWordBoundary
	| matcher |
	matcher := self matcherClass forString: '\B'.
	self
		assert: (matcher copy: 'foo bar' replacingMatchesWith: '*')
			= 'f*o*o b*a*r'
		description: 'An empty string not at a word boundary'