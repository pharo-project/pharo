tests parsing
testPragmaImplicitLiteralArrayIsInvalid
	| pragmaWithExpressionAsArgument pragmaWithLiteralArrayAsArgument pragmaWithStringAsArgument |
	"a pragma with arguments that look like code
	(3+4) were implicitly converted to a literal array
	#(3+4), this is not allowed anymore"
	pragmaWithExpressionAsArgument := 'foo 
	<func: (3+4) res: 7>
	^ self'.
	self should:[RBParser parseMethod: pragmaWithExpressionAsArgument] raise: Exception.

	"an explicit literal array is allowed"
	pragmaWithLiteralArrayAsArgument := 'foo 
	<func: #(3+4) res: 7>
	^ self'.
	"Should work"
	RBParser parseMethod: pragmaWithLiteralArrayAsArgument.

	"and of course a string literal"
	pragmaWithStringAsArgument := 'foo 
	<func: ''(3+4)'' res: 7>
	^ self'.
	"should work"
	RBParser parseMethod: pragmaWithStringAsArgument.
