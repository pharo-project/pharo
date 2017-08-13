tests - instance creation
testCreationFromBytes3
	"self run: #testCreationFromBytes3"

	"it is illegal for a LargeInteger to be less than SmallInteger maxVal." 
	"here we test that Integer>>byte!byte2:byte3:byte4: resconstructs (SmallInteger maxVal - 1) as an instance of SmallInteger. "
	| maxSmallInt hexString byte1 byte2 byte3 byte4 builtInteger |
	Smalltalk vm wordSize = 4 ifFalse: [ ^ self skip ].
	maxSmallInt := SmallInteger maxVal.
	hexString := (maxSmallInt - 1) printStringHex.
	self assert: hexString size = 8.
	byte4 := Number readFrom: (hexString copyFrom: 1 to: 2) base: 16.
	byte3 := Number readFrom: (hexString copyFrom: 3 to: 4) base: 16.
	byte2 := Number readFrom: (hexString copyFrom: 5 to: 6) base: 16.
	byte1 := Number readFrom: (hexString copyFrom: 7 to: 8) base: 16.
	builtInteger := Integer byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4.
	self assert: builtInteger = (maxSmallInt - 1).
	self assert: builtInteger class = SmallInteger
