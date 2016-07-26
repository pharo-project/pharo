tests substrings
testFindingTextWithoutJunkcharacter

	| intervalResult |
	intervalResult := MessageBrowser new searchedString: #LayoutFrame in: 'foo
asLayoutFrame

LayoutFrame '.

	self assert: (intervalResult = (20 to: 30))