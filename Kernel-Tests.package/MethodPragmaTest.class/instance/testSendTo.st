testing
testSendTo
	| pragma wasHere |
	pragma := Pragma keyword: #value:value: arguments: #( 1 2 ).
	self assert: (pragma sendTo: [ :a :b |
		self assert: a = 1; assert: b = 2.
		wasHere := true ]).
	self assert: wasHere