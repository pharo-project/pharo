accessing
roundedCorners: anArray
	"Set the corners to round."

	super roundedCorners: anArray.
	self contentMorph ifNotNil: [:cm | cm roundedCorners: self roundedCorners]