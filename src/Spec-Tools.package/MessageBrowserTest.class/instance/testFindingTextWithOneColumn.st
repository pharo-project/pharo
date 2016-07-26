tests
testFindingTextWithOneColumn

	| intervalResult |
	intervalResult := MessageBrowser new searchedString: #foo: in: 'zork
self foo: 3.
self foo: 6 '.

	self assert: (intervalResult = (11 to: 14))