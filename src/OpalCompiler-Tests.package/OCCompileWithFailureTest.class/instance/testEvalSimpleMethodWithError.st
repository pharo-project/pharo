tests
testEvalSimpleMethodWithError
	| ast cm |
	ast := OpalCompiler new
				source: 'method 3+';
				options: #(+ optionParseErrors);
				parse.
	
	self assert: ast isMethod.
	self assert: ast isFaulty.
	
	cm := ast compiledMethod.
	self should: [cm valueWithReceiver: nil arguments: #()] raise: SyntaxErrorNotification