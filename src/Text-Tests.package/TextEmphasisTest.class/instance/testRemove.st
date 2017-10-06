tests
testRemove
	| t1  |
	t1 := TextEmphasis bold.
	t1 add: TextEmphasis italic.
	t1 remove: TextEmphasis bold.
	self assert: t1 = TextEmphasis italic description: 'Removing bold didn''t leave italics'