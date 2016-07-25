tests - line breaking
testLinesDoWithCr

	| myString  |
	myString := 'This is a test of line breaking', String cr.
	myString linesDo: [ :line |
		self assert: ((line size + 1) = myString size)]