testing-replacing
testReplaceMethodUnary
	| tree |
	tree := self parseMethod: 'run "1" ^ "2" 3 "4"'.
	tree renameSelector: #runCase andArguments: #().
	self assert: tree newSource = 'runCase "1" ^ "2" 3 "4"'.
	
	tree := self parseMethod: 'run "1" ^ "2" 3 "4"'.
	tree renameSelector: #~~ andArguments: (Array with: (self parseExpression: 'first')).
	self assert: tree newSource = '~~ first "1" ^ "2" 3 "4"'.

	tree := self parseMethod: 'run "1" ^ "2" 3 "4"'.
	tree renameSelector: #assert: andArguments: (Array with: (self parseExpression: 'first')).
	self assert: tree newSource = 'assert: first "1" ^ "2" 3 "4"'.

	tree := self parseMethod: 'run "1" ^ "2" 3 "4"'.
	tree renameSelector: #assert:description: andArguments: (Array with: (self parseExpression: 'first') with: (self parseExpression: 'second')).
	self assert: tree newSource = 'assert: first description: second "1" ^ "2" 3 "4"'