tests - sunit original tests
testAdd 
	| e |
	e := self element.
	empty add: e.
	self assert: (empty includes: e) 
			description: ['after adding ' , e printString , ', it wasn''t included']