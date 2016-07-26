tests - IEEE 754
test32bitRoundingMode
	"method asIEEE32BitWord did not respect IEEE default rounding mode"
	
	| conv expected exponentPart |
	
	"IEEE 32 bits Float have 1 bit sign/8 bit exponent/23 bits of mantissa after leading 1
	2r1.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2reeeeeeee-127) * sign
	except when 2reeeeeeee isZero, which is a gradual underflow:
	2r0.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2r00000000-127) * sign
	and when 2reeeeeeee = 255, which is infinity if mantissa all zero or nan otherwise"
	
	"This example has two extra bits in mantissa for testing rounding mode
	case 1: should obviously round to upper"
	conv := 2r1.0000000000000000000000111e25 asIEEE32BitWord.
	
	"expected float encoded as sign/exponent/mantissa (whithout leading 1)"
	exponentPart := 25+127 bitShift: 23. "127 is 2r01111111 or 16r7F"
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 2: exactly in the mid point of two 32 bit float: round toward nearest even (to upper)"
	conv := 2r1.0000000000000000000000110e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 3: exactly in the mid point of two 32 bit float: round toward nearest even (to lower)"
	conv := 2r1.0000000000000000000000010e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r0.
	self assert: expected = conv.
	
	"case 4: obviously round to upper"
	conv := 2r1.0000000000000000000000011e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r1.
	self assert: expected = conv.
