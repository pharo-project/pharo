testing-compiler
testCompileCharacter
	self assertPragma: 'foo: $a' givesKeyword: #foo: arguments: #( $a ).
	self assertPragma: 'foo: $ ' givesKeyword: #foo: arguments: #( $  ).