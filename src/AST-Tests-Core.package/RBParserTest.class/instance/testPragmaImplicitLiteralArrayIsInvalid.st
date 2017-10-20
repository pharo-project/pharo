tests parsing
testPragmaImplicitLiteralArrayIsInvalid
	| pragmaWithExpressionAsArgument pragmaWithLiteralArrayAsArgument pragmaWithStringAsArgument |
	"a pragma with arguments that look like code
	(3+4) were implicitly converted to a literal array
	#(3+4), this is not allowed anymore"
	pragmaWithExpressionAsArgument := 'foo 
	<func: (3+4) res: 7>
	^ self'.
	self should:[self parserClass parseMethod: pragmaWithExpressionAsArgument] raise: Exception.

	"an explicit literal array is allowed"
	pragmaWithLiteralArrayAsArgument := 'foo 
	<func: #(3+4) res: 7>
	^ self'.
	"Should work"
	self parserClass parseMethod: pragmaWithLiteralArrayAsArgument.

	"and of course a string literal"
	pragmaWithStringAsArgument := 'foo 
	<func: ''(3+4)'' res: 7>
	^ self'.
	"should work"
	self parserClass parseMethod: pragmaWithStringAsArgument.
