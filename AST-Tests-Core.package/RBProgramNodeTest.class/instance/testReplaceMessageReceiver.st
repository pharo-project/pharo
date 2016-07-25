testing-replacing
testReplaceMessageReceiver
	| tree |
	tree := self parseMethod: 'foo "1" self "2" foo: "3" 123 "4"'.
	tree body statements first receiver replaceWith: (self parseExpression: 'bar').
	self assert: tree newSource = 'foo "1" bar "2" foo: "3" 123 "4"'.
	tree := self parseMethod: 'foo "1" self "2" foo: "3" 123 "4"'.
	tree body statements first receiver replaceWith: (self parseExpression: 'bar msg1 msg2').
	self assert: tree newSource = 'foo "1" bar msg1 msg2 "2" foo: "3" 123 "4"'.
	tree := self parseMethod: 'foo "1" self foo "2" foo: "3" 123 "4"'.
	tree body statements first receiver replaceWith: (self parseExpression: 'bar').
	self assert: tree newSource = 'foo "1" bar "2" foo: "3" 123 "4"'