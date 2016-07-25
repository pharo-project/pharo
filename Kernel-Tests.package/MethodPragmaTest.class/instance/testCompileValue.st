testing-compiler
testCompileValue
	self assertPragma: 'foo: true' givesKeyword: #foo: arguments: #( true ).
	self assertPragma: 'foo: false' givesKeyword: #foo: arguments: #( false ).
	self assertPragma: 'foo: nil' givesKeyword: #foo: arguments: #( nil )