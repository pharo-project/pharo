tests
testAllLocalCallsOn
	" self debug: #testAllLocalCallsOn"
	
	self assert: (( Point allLocalCallsOn: #asPoint )  notEmpty).
	self assert: (( Point allLocalCallsOn: #asPoint )  size = 4).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point>> #roundDownTo:) asRingDefinition).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point >> #roundUpTo:) asRingDefinition).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point >> #roundTo:) asRingDefinition).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point >> #truncateTo: ) asRingDefinition).
	