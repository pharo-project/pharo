accessing
color
	"Return the current fill color as a Color.  
	 Gives the wrong answer if the halftoneForm is a complex pattern of more than one word."

	halftoneForm ifNil: [^ Color black].
	^ Color colorFromPixelValue: halftoneForm first depth: destForm depth