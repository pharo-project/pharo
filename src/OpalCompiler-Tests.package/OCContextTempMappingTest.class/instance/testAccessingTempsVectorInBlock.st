tests
testAccessingTempsVectorInBlock

	[ | temp |
		[ temp := 1.
		self assert: ((thisContext tempNamed: 'temp') == 1) ] value.
	] value