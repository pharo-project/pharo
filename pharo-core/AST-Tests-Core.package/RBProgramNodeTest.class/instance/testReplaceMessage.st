testing-replacing
testReplaceMessage
	| tree |
	tree := self parseMethod: 'run "1" self "2" run "3"'.
	tree body statements first replaceWith: (self parseExpression: 'self runCase').
	self assert: tree newSource = 'run "1" self "2" runCase "3"'