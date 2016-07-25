tests
testFindingNotExistingText

	| intervalResult |
	intervalResult := MessageBrowser new searchedString: #foo in: 'zork
hello world '.

	self assert: (intervalResult = (0 to: 0))