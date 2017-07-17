tests parsing
testSymbolLiteral
	| tree |
	#(
	'#<' #<
	'#>' #>
	'#<>' #<>
	'##foo' #foo 
	'###foo' #foo 
	'#foo:' #foo:
	'#foo::' #'foo::'
	'#foo::bar' #'foo::bar'
	'#foo::bar:' #'foo::bar:'
	'#foo::bar::' #'foo::bar::') pairsDo: [ :parseString :expectedResult |
		tree := self parserClass parseExpression: parseString.
		self assert: tree value equals: expectedResult.		
		self assert: tree start equals: 1.
		self assert: tree stop equals: parseString size ].
	
	#(
	('#"bar"foo' #foo)
	('# foo' #foo)
	('#1' 1)
	('#12' 12)
	('#12.3' 12.3)
	('# 1' 1)
	('##1' 1)
	('#"bar"1' 1)) do: [ :pair | 
		self should: [self parserClass parseExpression: pair first] raise: SyntaxErrorNotification ]