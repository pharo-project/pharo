tests - line breaking
testLinesDoWithCrLfBetween

	| aString |
	aString := '-This is a test-', String crlf,'of line breaking'.
	aString linesDo: [ :line |
		self assert: ((line size) = 16)]