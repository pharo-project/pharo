tests
testParseTreeTypes
	| types model |
	model := RBClassModelFactory rbNamespace new.
	types := RBRefactoryTyper 
				typesFor: 'foo'
				in: (RBParser 
						parseExpression: 'foo printString; testBasicLintRuleTypes; testParseTreeTypes')
				model: model.
	self assert: types size = 1.
	self assert: (types includes: (model classFor: self class))