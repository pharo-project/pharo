testing-compiler
testCompileBinary
	self assertPragma: ' = 1' givesKeyword: #= arguments: #( 1 ).
	self assertPragma: ' , 3' givesKeyword: #, arguments: #( 3 ).
	self assertPragma: ' > 4' givesKeyword: #> arguments: #( 4 ).
	self assertPragma: ' < 5' givesKeyword: #< arguments: #( 5 ).

	self assertPragma: ' == 1' givesKeyword: #== arguments: #( 1 ).
	self assertPragma: ' <> 3' givesKeyword: #<> arguments: #( 3 ).
	self assertPragma: ' >< 4' givesKeyword: #>< arguments: #( 4 ).
	self assertPragma: ' ** 5' givesKeyword: #** arguments: #( 5 )