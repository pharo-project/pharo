copying
pixelAt: aPoint put: pixelValue 
	"Assumes this BitBlt has been set up specially (see the init message,
	BitBlt bitPokerToForm:.  Overwrites the pixel at aPoint."
	destX := aPoint x.
	destY := aPoint y.
	sourceForm unhibernate.	"before poking"
	sourceForm bits 
		at: 1
		put: pixelValue.
	self copyBits
	"
| bb |
bb := (BitBlt bitPokerToForm: Display).
[Sensor anyButtonPressed] whileFalse:
	[bb pixelAt: Sensor cursorPoint put: 55]
"