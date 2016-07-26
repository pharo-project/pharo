testing-protocol
testCopyReplacingMatches
	"See that the match context is preserved while copying stuff between matches:"
	
	| matcher |
	matcher := self matcherClass forString: '\<\d\D+'.
	self assert: (matcher copy: '9aaa1bbb 8ccc' replacingMatchesWith: 'foo')
		= 'foo1bbb foo'