testing-compiler
testCompileString
	self assertPragma: 'foo: ''''' givesKeyword: #foo: arguments: #( '' ).
	self assertPragma: 'foo: ''bar''' givesKeyword: #foo: arguments: #( 'bar' ).