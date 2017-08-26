tests parsing
testInvalidPragmaPosition
	#(
	'foo self. <foo: $a>'
	'<foo: $a>'
	'foo [:x | <foo: $a> ] value'
	'foo ^ <foo: $a>'
	) 
	do: [ :each | self should: [ self parserClass parseMethod: each ] raise: SyntaxErrorNotification ]