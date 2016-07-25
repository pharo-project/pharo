tests - IEEE 754
test32bitGradualUnderflow
	"method asIEEE32BitWord did not respect IEEE gradual underflow"
	
	| conv expected exponentPart |
	
	"IEEE 32 bits Float have 1 bit sign/8 bit exponent/23 bits of mantissa after leading 1
	2r1.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2reeeeeeee-127) * sign
	except when 2reeeeeeee isZero, which is a gradual underflow:
	2r0.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2r00000000-126) * sign
	and when 2reeeeeeee = 255, which is infinity if mantissa all zero or nan otherwise"
	
	"case 1: This example is the first gradual underflow case"
	conv := 2r0.11111111111111111111111e-126 asIEEE32BitWord.
	
	"expected float encoded as sign/exponent/mantissa (whithout leading 1 or 0)"
	exponentPart := 0.
	expected := exponentPart bitOr: 2r11111111111111111111111.
	self assert: expected = conv.
	
	"case 2: smallest number"
	conv := 2r0.00000000000000000000001e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r1.
	self assert: expected = conv.
	
	"case 3: round to nearest even also in underflow cases... here round to upper"
	conv := 2r0.000000000000000000000011e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 4: round to nearest even also in underflow cases... here round to lower"
	conv := 2r0.000000000000000000000101e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 5: round to nearest even also in underflow cases... here round to upper"
	conv := 2r0.0000000000000000000001011e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r11.
	self assert: expected = conv.
	