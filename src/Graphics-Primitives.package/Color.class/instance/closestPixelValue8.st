conversions
closestPixelValue8
	"Return the nearest approximation to this color for an 8-bit deep Form."

	"fast special cases"
	rgb = 0 ifTrue: [^ 1].  "black"
	rgb = 16r3FFFFFFF ifTrue: [^ 255].  "white"

	self saturation < 0.2 ifTrue: [
		^ GrayToIndexMap at: (self privateGreen >> 2) + 1.  "nearest gray"
	] ifFalse: [
		"compute nearest entry in the color cube"
		^ 40 +
		  ((((self privateRed * 5) + HalfComponentMask) // ComponentMask) * 36) +
		  ((((self privateBlue * 5) + HalfComponentMask) // ComponentMask) * 6) +
		  (((self privateGreen * 5) + HalfComponentMask) // ComponentMask)].
