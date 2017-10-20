testing-compiler
testCompileNumber
	self assertPragma: 'foo: 123' givesKeyword: #foo: arguments: #( 123 ).
	self assertPragma: 'foo: -123' givesKeyword: #foo: arguments: #( -123 ).
	self assertPragma: 'foo: 12.3' givesKeyword: #foo: arguments: #( 12.3 ).
	self assertPragma: 'foo: -12.3' givesKeyword: #foo: arguments: #( -12.3 ).