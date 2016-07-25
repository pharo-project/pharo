testing-pragma
testMessage
	| pragma message |
	pragma := Pragma keyword: #foo: arguments: #( 123 ).
	message := pragma message.
	
	self assert: message selector = #foo:.
	self assert: message arguments = #( 123 ).