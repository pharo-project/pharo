testing-pragma
testKeyword
	| pragma |
	pragma := Pragma keyword: #foo: arguments: #( 123 ).
	self assert: pragma keyword = #foo:.