transformations
- aColor
	"Answer aColor is subtracted from the given color in an additive color space.  "
	"(Color white - Color red) display"

	^ self class basicNew
		setPrivateRed: self privateRed - aColor privateRed
		green: self privateGreen - aColor privateGreen
		blue: self privateBlue - aColor  privateBlue;
		setAlpha: ((self = aColor) ifTrue: [ 0 ] ifFalse: [ self alpha ])
