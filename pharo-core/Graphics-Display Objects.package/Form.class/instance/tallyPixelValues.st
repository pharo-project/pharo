analyzing
tallyPixelValues
	"Answer a Bitmap whose elements contain the number of pixels in this Form with the pixel value corresponding to their index. Note that the pixels of multiple Forms can be tallied together using tallyPixelValuesInRect:into:."

	^ self tallyPixelValuesInRect: self boundingBox
		into: (Bitmap new: (1 bitShift: (self depth min: 15)))
"
Move a little rectangle around the screen and print its tallies...
 | r tallies nonZero |
Cursor blank showWhile: [
[Sensor anyButtonPressed] whileFalse:
	[r := Sensor cursorPoint extent: 10@10.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil.
	tallies := (Display copy: r) tallyPixelValues.
	nonZero := (1 to: tallies size) select: [:i | (tallies at: i) > 0]
			thenCollect: [:i | (tallies at: i) -> (i-1)].
	nonZero printString , '          ' displayAt: 0@0.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil]]
"
