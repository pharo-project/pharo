testing-pragma
testArguments
	| pragma |
	pragma := Pragma keyword: #foo: arguments: #( 123 ).
	self assert: pragma arguments = #( 123 ).