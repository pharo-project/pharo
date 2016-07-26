tests
testFindingText

	| intervalResult |
	intervalResult := MessageBrowser new searchedString: #LayoutFrame in: 'foo
LayoutFrame

asLayoutFrame '.

	self assert: (intervalResult = (5 to: 15))