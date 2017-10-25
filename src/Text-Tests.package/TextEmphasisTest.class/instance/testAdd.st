tests
testAdd
	| t1  |
	t1 := TextEmphasis bold.
	t1 add: TextEmphasis italic.
	self assert: (t1 = 2r11) description: 'bold and italics didn''t add correctly'