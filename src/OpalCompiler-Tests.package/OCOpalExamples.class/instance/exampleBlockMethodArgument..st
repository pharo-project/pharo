examples-blocks
exampleBlockMethodArgument: arg
	|  block |
	block := [ :blockarg | blockarg + arg].
	self assert: ((block value: 2) = (arg + 2)).
	^block value: 2