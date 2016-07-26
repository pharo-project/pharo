*Morphic-Base
fromUser: gridPoint 
	"Answer a Rectangle that is determined by having the user 
	designate the top left and bottom right corners. 
	The cursor reamins linked with the sensor, but
	the outline is kept gridded."
	| originRect |
	self activeHand showTemporaryCursor: Cursor origin.

	originRect :=  ((self activeHand cursorPoint grid: gridPoint) extent: 0 @ 0) 
					newRectFrom: [ :f :pt | (pt grid: gridPoint) extent: 0 @ 0 ].
					
	self activeHand showTemporaryCursor: Cursor bottomRight.

	^  originRect newRectFrom: [ :f :pt | f origin corner: (pt grid: gridPoint) ]