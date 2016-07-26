testing-replacing
testReplaceMethodKeywordLong
	| tree |
	tree := self parseMethod: 'deny: "1" anObject "2" description: "3" anotherObject "4" ^ "5" 6 "7"'.
	tree renameSelector: #runCase andArguments: #().
	self assert: tree newSource = 'runCase "4" ^ "5" 6 "7"'.
	
	tree := self parseMethod: 'deny: "1" anObject "2" description: "3" anotherObject "4" ^ "5" 6 "7"'.
	tree renameSelector: #~~ andArguments: (Array with: (self parseExpression: 'first')).
	self assert: tree newSource = '~~ first "4" ^ "5" 6 "7"'.

	tree := self parseMethod: 'deny: "1" anObject "2" description: "3" anotherObject "4" ^ "5" 6 "7"'.
	tree renameSelector: #assert: andArguments: (Array with: (self parseExpression: 'first')).
	self assert: tree newSource = 'assert: first "4" ^ "5" 6 "7"'.

	tree := self parseMethod: 'deny: "1" anObject "2" description: "3" anotherObject "4" ^ "5" 6 "7"'.
	tree renameSelector: #assert:description: andArguments: (Array with: (self parseExpression: 'first') with: (self parseExpression: 'second')).
	self assert: tree newSource = 'assert: "1" first "2" description: "3" second "4" ^ "5" 6 "7"'