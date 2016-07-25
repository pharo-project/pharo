tests parsing
testParsingLiteralMessages
	self assert: (RBParser parseExpression: 'nil self nil') isMessage.
	self assert: (RBParser parseExpression: 'self true') isMessage.
	self assert: (RBParser parseExpression: 'self false') isMessage.
	self assert: (RBParser parseExpression: 'self -1') isMessage.
	
	self assert: (RBParser parseMethod: 'nil') isMethod.
	self assert: (RBParser parseMethod: 'true') isMethod.
	self assert: (RBParser parseMethod: 'false') isMethod