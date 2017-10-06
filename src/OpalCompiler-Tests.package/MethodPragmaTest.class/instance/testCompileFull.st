testing-compiler
testCompileFull
	self assertPragma: 'foo: 1' givesKeyword: #foo: arguments: #( 1 ).
	self assertPragma: 'foo: 1 bar: 2' givesKeyword: #foo:bar: arguments: #( 1 2 ).