stop conditions
crossedX
	"This condition will sometimes be reached 'legally' during display, when, 
	for instance the space that caused the line to wrap actually extends over 
	the right boundary. This character is allowed to display, even though it 
	is technically outside or straddling the clipping rectangle since it is in 
	the normal case not visible and is in any case appropriately clipped by 
	the scanner."

	self advanceIfFirstCharOfLine.
	lastDisplayableIndex := lastIndex - 1.
	^ true 