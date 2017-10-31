tests - instance creation
testLoremIpsum2048
	| fillerText |
	fillerText := String loremIpsum: 2048.
	self assert: fillerText size equals: 2048.
	self assert: (fillerText beginsWith: 'Lorem ipsum').
	self assert: (fillerText includesSubstring: 'laborum.').
	self assert: (fillerText endsWith: ('.', String cr)).
	self assert: fillerText lines size > 2.
	self assert: fillerText isAsciiString.
	self assert: (Character space split: fillerText) size > 75