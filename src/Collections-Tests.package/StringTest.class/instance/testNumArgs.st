tests
testNumArgs
	"This is about http://code.google.com/p/pharo/issues/detail?id=237"
	
	| binary |
	binary := '*+-/\~=<>&@%,|'.
	self assert: (binary allSatisfy: [:char1 |
			(String with: char1) numArgs = 1 and: [binary allSatisfy: [:char2 |
				(String with: char1 with: char2) numArgs = 1 and: [binary allSatisfy: [:char3 |
					(String with: char1 with: char2 with: char3) numArgs = 1]]]]])
		description: 'binary selectors have 1 argument'.
		
	self assert: 'x' numArgs = 0
		description: 'unary selectors have 0 arguments'.
	self assert: 'x0' numArgs = 0
		description: 'unary selectors have 0 arguments'.
	self assert: 'yourself' numArgs = 0
		description: 'unary selectors have 0 arguments'.
		
	self assert: 'x:' numArgs = 1
		description: 'keyword selectors have as many elements as colons characters'.
	self assert: 'x:y:' numArgs = 2
		description: 'keyword selectors have as many elements as colons characters'.
	self assert: 'at:put:' numArgs = 2
		description: 'keyword selectors have as many elements as colons characters'.
			
	self assert: 'x_y_z' numArgs = 0
		description: 'selectors can contain underscores'.
	self assert: '_x' numArgs = 0
		description: 'selectors can begin with an underscore'.
	self assert: '_:_:' numArgs = 2
		description: 'keyword selectors can contain underscores'.
		
	self assert: 'at:withoutTrailingColon' numArgs = -1
		description: 'keyword selectors should have a trailing colon character'.
		
	self assert: ':x' numArgs = -1
		description: 'keyword selectors cannot begin with a colon character'.
		
	self assert: 'x::y:' numArgs = -1
		description: 'keyword selectors cannot have two consecutive colon characters'.
		
	self assert: '0x' numArgs = -1
		description: 'selectors cannot begin with a digit'.
		
	self assert: 'x::0y:' numArgs = -1
		description: 'keyword selectors cannot have any key beginning with a digit'.
	
	