tests - instance creation
testStringAsNumber
	"This covers parsing in Number>>readFrom:
	Trailing decimal points should be ignored."

	self assert: ('123' asNumber = 123).
	self assert: ('-123' asNumber = -123).
	self assert: ('123.' asNumber = 123).
	self assert: ('-123.' asNumber = -123).
	self assert: ('123This is not to be read' asNumber = 123).
	self assert: ('123s could be confused with a ScaledDecimal' asNumber = 123).
	self assert: ('123e could be confused with a Float' asNumber = 123).
