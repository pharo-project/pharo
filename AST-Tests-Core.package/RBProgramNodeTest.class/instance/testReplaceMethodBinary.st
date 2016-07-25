testing-replacing
testReplaceMethodBinary
	| tree |
	tree := self parseMethod: '= "1" anObject "2" ^ "3" 4 "5"'.
	tree renameSelector: #runCase andArguments: #().
	self assert: tree newSource = 'runCase "2" ^ "3" 4 "5"'.
	
	tree := self parseMethod: '= "1" anObject "2" ^ "3" 4 "5"'.
	tree renameSelector: #~~ andArguments: (Array with: (self parseExpression: 'first')).
	self assert: tree newSource = '~~ "1" first "2" ^ "3" 4 "5"'.

	tree := self parseMethod: '= "1" anObject "2" ^ "3" 4 "5"'.
	tree renameSelector: #assert: andArguments: (Array with: (RBVariableNode named: 'first')).
	self assert: tree newSource = 'assert: "1" first "2" ^ "3" 4 "5"'.

	tree := self parseMethod: '= "1" anObject "2" ^ "3" 4 "5"'.
	tree renameSelector: #assert:description: andArguments: (Array with: (RBVariableNode named: 'first') with: (RBVariableNode named: 'second')).
	self assert: tree newSource = 'assert: first description: second "2" ^ "3" 4 "5"'