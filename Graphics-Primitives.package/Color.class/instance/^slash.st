transformations
/ aNumber
	"Answer this color with its RGB divided by the given number. "
	"(Color red / 2) display"

	^ self class basicNew
		setPrivateRed: (self privateRed / aNumber) asInteger
		green: (self privateGreen / aNumber) asInteger
		blue: (self privateBlue / aNumber) asInteger;
		setAlpha: self alpha
