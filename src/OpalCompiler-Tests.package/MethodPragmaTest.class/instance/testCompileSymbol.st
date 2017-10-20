testing-compiler
testCompileSymbol
	self assertPragma: 'foo: #bar' givesKeyword: #foo: arguments: #( bar ).
	self assertPragma: 'foo: #bar:' givesKeyword: #foo: arguments: #( bar: ).
	self assertPragma: 'foo: #bar:zork:' givesKeyword: #foo: arguments: #( bar:zork: ).