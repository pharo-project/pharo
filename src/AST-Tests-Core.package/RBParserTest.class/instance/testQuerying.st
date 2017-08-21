tests
testQuerying
	| tree aNode arg1Node bNode |
	tree := self parserClass 
				parseMethod: ('test: a`	| b |`	b := (self foo: a; bar) baz.`	b := super test: b.`	^[:arg1 | self foa1 + (super foo: arg1 foo: a foo: b)]' 
						copyReplaceAll: '`'
						with: (String with: (Character value: 13))).
	self 
		assert: tree selfMessages asSortedCollection asArray = #(#bar #foa1 #foo:).
	self assert: tree superMessages asSortedCollection asArray 
				= #(#foo:foo:foo: #test:).
	aNode := tree whichNodeIsContainedBy: (112 to: 112).
	self assert: aNode name = 'a'.
	bNode := tree whichNodeIsContainedBy: (119 to: 119).
	self assert: bNode name = 'b'.
	arg1Node := tree whichNodeIsContainedBy: (102 to: 105).
	self assert: arg1Node name = 'arg1'.
	self assert: (arg1Node statementNode isMessage 
				and: [arg1Node statementNode selector = #+]).
	self assert: (arg1Node whoDefines: 'arg1') isBlock.
	self assert: (aNode whoDefines: 'a') isMethod.
	self assert: (aNode whoDefines: 'b') isSequence.
	self assert: (tree whichNodeIsContainedBy: (91 to: 119)) selector 
				= #foo:foo:foo:.
	self assert: (tree whichNodeIsContainedBy: (69 to: 121)) isBlock.
	self assert: (tree whichNodeIsContainedBy: (69 to: 118)) isNil.
	self assert: aNode blockVariables asSortedCollection asArray = #('arg1').
	self assert: aNode temporaryVariables asSortedCollection asArray = #('b').
	self assert: tree allDefinedVariables asSortedCollection asArray 
				= #('a' 'arg1' 'b').
	self assert: tree allArgumentVariables asSortedCollection asArray 
				= #('a' 'arg1').
	self 
		assert: tree allTemporaryVariables asSortedCollection asArray = #('b')