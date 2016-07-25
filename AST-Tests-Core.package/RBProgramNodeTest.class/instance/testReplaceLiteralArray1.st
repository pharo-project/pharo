testing-replacing
testReplaceLiteralArray1
	| tree |
	tree := self parseMethod: 'run "1" #(1 2 3) "2"'.
	tree body statements first replaceWith: (self parseExpression: '#[1 2 3]').
	self assert: tree newSource = 'run "1" #[1 2 3] "2"'.
	tree := self parseMethod: 'run "1" #(1 2 3) "2"'.
	tree body statements first replaceWith: (self parseExpression: '123').
	self assert: tree newSource = 'run "1" 123 "2"'