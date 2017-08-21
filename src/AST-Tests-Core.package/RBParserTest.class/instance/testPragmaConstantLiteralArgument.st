tests parsing
testPragmaConstantLiteralArgument
	| primitiveDeclartion validPragma invalidPragma |
	"primitive declarations with variable (code) are allowed"
	primitiveDeclartion := 'foo 
	<primitive: #dooo module: #bar error: code>
	^ nil'.
	"#Point as argument is allowed"
	validPragma := 'foo 
	<return: #Point>
	^ 0@0'.
	"Point as argument is not allowed"
	invalidPragma := 'foo 
	<return: Point>
	^ 0@0'.
	self shouldnt:[self parserClass parseMethod: primitiveDeclartion] raise: Exception.
	self shouldnt:[self parserClass 	parseMethod: validPragma] raise: Exception.
	self should:[self parserClass parseMethod: invalidPragma] raise: Exception.
	
