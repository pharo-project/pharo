displaying
displayLine: aString at: aPoint 
	"Display the characters in aString, starting at position aPoint."

	self characters: (1 to: aString size)
		in: aString
		displayAt: aPoint
		clippedBy: Display boundingBox
		rule: Form over
		fillColor: nil
		kernDelta: 0
		on: (BitBlt toForm: Display).
