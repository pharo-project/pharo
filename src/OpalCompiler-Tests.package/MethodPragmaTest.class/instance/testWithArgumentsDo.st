testing
testWithArgumentsDo
	| pragma wasHere |
	pragma := Pragma keyword: #add:after: arguments: #( 1 2 ).
	self assert: (pragma withArgumentsDo: [ :a :b |
		self assert: a = 1; assert: b = 2.
		wasHere := true ]).
	self assert: wasHere