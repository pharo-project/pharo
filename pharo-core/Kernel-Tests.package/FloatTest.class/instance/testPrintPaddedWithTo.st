tests - printing
testPrintPaddedWithTo
	"The problem was caused by treating the format specifier as a number rather than
	as a string, such the the number may be a Float subject to floating point rounding
	errors. The solution to treat the format specifier as a string, and extract the integer
	fields before and after the decimal point in the string."

	self assert: (1.0 printPaddedWith: $0 to: 2.2) = '01.00'.
	self assert: (1.0 printPaddedWith: $X to: 2.2) = 'X1.0X'.
	self assert: (1.0 printPaddedWith: $0 to: 2) = '01.0'.
	self assert: (12345.6789 printPaddedWith: $0 to: 2) = '12345.6789'.
	self assert: (12345.6789 printPaddedWith: $0 to: 2.2) = '12345.6789'.
	self assert: (12.34 printPaddedWith: $0 to: 2.2) = '12.34'.
	self assert: (12345.6789 printPaddedWith: $0 to: 2.2) = '12345.6789'.
	self assert: (123.456 printPaddedWith: $X to: 4.4) = 'X123.456X'.
	self assert: (1.0 printPaddedWith: $0 to: 2.1) = '01.0'.
	self assert: (1.0 printPaddedWith: $0 to: 2.2) = '01.00'.
	self assert: (1.0 printPaddedWith: $0 to: 2.3) = '01.000'. "previously failed due to float usage"
	self assert: (1.0 printPaddedWith: $0 to: 2.4) = '01.0000'. "previously failed due to float usage"
	self assert: (1.0 printPaddedWith: $0 to: 2.5) = '01.00000'

