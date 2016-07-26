testing-replacing
testReplaceVariable
	| tree |
	tree := self parseMethod: 'run "1" foo "2"'.
	tree body statements first replaceWith: (self parseExpression: 'zork').
	self assert: tree newSource = 'run "1" zork "2"'.
	tree := self parseMethod: 'run "1" foo "2"'.
	tree body statements first replaceWith: (self parseExpression: '123').
	self assert: tree newSource = 'run "1" 123 "2"'