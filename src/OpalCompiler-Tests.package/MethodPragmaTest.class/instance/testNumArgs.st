testing
testNumArgs
	| pragma |
	pragma := Pragma keyword: #value arguments: #().
	self assert: pragma numArgs = 0.
	
	pragma := Pragma keyword: #+ arguments: #( 1 ).
	self assert: pragma numArgs = 1.
	
	pragma := Pragma keyword: #value:value: arguments: #( 1 2 ).
	self assert: pragma numArgs = 2