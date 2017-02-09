tests
testSplitJoinBoundaryCases
	"Empty splitter, joiner or sequence."
	self assert: ('' join: ('.' asRegex split: '')) equals: ''. "NB: Doesn't work with empty regex"