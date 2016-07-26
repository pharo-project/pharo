accessing
roundedCorners: anArray
	"Set the corners to round."

	super roundedCorners: anArray.
	self buttonMorph ifNotNil: [:b |
		b roundedCorners: (anArray copyWithoutAll: #(1 2))]