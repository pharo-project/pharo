filling
bitPatternForDepth: suspectedDepth
	"Only called when a Form is being used as a fillColor.  Use a Pattern or InfiniteForm instead for this purpose.
	Interpret me as an array of (32/depth) Color pixelValues.  BitBlt aligns the first element of this array with the top scanline of the destinationForm, the second with the second, and so on, cycling through the color array as necessary."

	^ self