dropping/grabbing
aboutToBeGrabbedBy: aHand 
	"The receiver is being grabbed by a hand.                           
	Perform necessary adjustments (if any) and return the actual morph    
	     that should be added to the hand."
	"Since this morph has been initialized automatically with bounds origin   
	     0@0, we have to move it to aHand position."
	super aboutToBeGrabbedBy: aHand.
	self draggedMorph.
	"self align: self bottomLeft with: aHand position."
	self formerPosition: self position.
	aHand newKeyboardFocus: self.