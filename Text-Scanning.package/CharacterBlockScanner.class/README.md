A CharacterScanner does scan text to compute the CharacterBlock for a character specified by its index in the text or its proximity to the cursor location. The CharacterBlock stores information both about character layout and character index in the text.

This class is essential for selecting text with the mouse or with arrow keys.

Instance Variables
	characterIndex:		<Integer | nil>
	characterPoint:		<Point>
	lastCharacterWidth:		<Number | nil>
	nextLeftMargin:		<Number>
	specialWidth:		<Number | nil>

characterIndex
	- the index of character for which the layout information is searched, or nil when the layout is searched by cursor location

characterPoint
	- the cursor location for which nearest character index and layout are searched.

lastCharacterWidth
	- a number indicating the width of last character being processed.
	Note that this variable is left to nil during the inner scan loop, and only set on stopConditions.

nextLeftMargin
	- a number specifying the distance between left of composition zone and left of first character for the next line.

specialWidth
	- a number holding the width of an embedded object if any, or nil if none.
