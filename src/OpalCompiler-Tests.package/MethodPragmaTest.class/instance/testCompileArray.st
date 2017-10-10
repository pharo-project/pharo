testing-compiler
testCompileArray
	self assertPragma: 'foo: #()' givesKeyword: #foo: arguments: #( () ).
	self assertPragma: 'foo: #( foo )' givesKeyword: #foo: arguments: #( ( foo ) ).
	self assertPragma: 'foo: #( foo: )' givesKeyword: #foo: arguments: #( ( foo: ) ).
	self assertPragma: 'foo: #( 12 )' givesKeyword: #foo: arguments: #( ( 12 ) ).
	self assertPragma: 'foo: #( true )' givesKeyword: #foo: arguments: #( ( true ) ).
	