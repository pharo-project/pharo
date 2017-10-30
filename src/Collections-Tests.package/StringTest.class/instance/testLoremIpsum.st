tests - instance creation
testLoremIpsum
	| fillerText |
	fillerText := String loremIpsum.
	self deny: fillerText isEmpty.
	self assert: (fillerText beginsWith: 'Lorem ipsum').
	self assert: (fillerText endsWith: 'laborum.').
	self assert: fillerText lines first equals: fillerText.
	self assert: fillerText isAsciiString.
	self assert: (Character space split: fillerText) size > 10