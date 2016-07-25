testing-replacing
testReplaceMessageArgument
	| tree |
	tree := self parseMethod: 'foo "1" self "2" foo: "3" foo "4"'.
	tree body statements first arguments first replaceWith: (self parseExpression: 'bar').
	self assert: tree newSource = 'foo "1" self "2" foo: "3" bar "4"'.
	tree := self parseMethod: 'foo "1" self "2" foo: "3" foo "4"'.
	tree body statements first arguments first replaceWith: (self parseExpression: 'bar msg1 msg2').
	self assert: tree newSource = 'foo "1" self "2" foo: "3" bar msg1 msg2 "4"'.
	tree := self parseMethod: 'foo "1" self "2" foo: "3" foo bar "4"'.
	tree body statements first arguments first replaceWith: (self parseExpression: 'bar').
	self assert: tree newSource = 'foo "1" self "2" foo: "3" bar "4"'