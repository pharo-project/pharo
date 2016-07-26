tests
testDigitValue
	"self debug: #testDigitValue"
	
	'0123456789ABCDEF' with: #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
		do: [:char :value | self assert: char digitValue = value].
	
	'0123456789' with: #(0 1 2 3 4 5 6 7 8 9)
		do: [:char :value | self assert: char digitValue = value].
		
	 