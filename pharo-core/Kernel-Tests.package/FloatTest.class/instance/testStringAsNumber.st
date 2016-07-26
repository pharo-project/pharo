tests - conversion
testStringAsNumber
	"This covers parsing in Number>>readFrom:"

	| aFloat |
	aFloat := '10r-12.3456' asNumber.
	self assert: -12.3456 = aFloat.
	aFloat := '10r-12.3456e2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '10r-12.3456d2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '10r-12.3456q2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '-12.3456q2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '12.3456q2' asNumber.
	self assert: 1234.56 = aFloat.
