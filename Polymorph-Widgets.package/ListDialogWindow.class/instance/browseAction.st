button behavior
browseAction
	| aString tmp block |
	aString := searchMorph content.
	list detect: [ :item | (self displayItem: item) = aString ] ifFound: [ :item | self accept: item ].
	tmp := answer.
	block := self browseBlock.
	[ block value: tmp ] fork.
	self cancel