tests - line breaking
testLinesDoWithCrLf

	| aString |
	aString := 'This is a test of line breaking', String crlf.
	aString linesDo: [ :line |
		self assert: ((line size + 2) = aString size)]