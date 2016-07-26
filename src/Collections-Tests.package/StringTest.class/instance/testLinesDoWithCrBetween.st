tests - line breaking
testLinesDoWithCrBetween

	| aString |
	aString := ' This is a test ', String cr,'of line breaking'.
	aString linesDo: [ :line |
		self assert: ((line size) = 16)]