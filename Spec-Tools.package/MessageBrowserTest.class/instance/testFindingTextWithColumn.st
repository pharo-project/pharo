tests
testFindingTextWithColumn

	| intervalResult |
	intervalResult := MessageBrowser new searchedString: #foo:bar: in: 'zork
self foo: 3 bar: 5.
self foo: 3 bar: 6 '.

	self assert: (intervalResult = (11 to: 21))