dropping/grabbing
aboutToBeGrabbedBy: aHand
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand."
	
	self formerOwner: owner.
	self formerPosition: self position.
	^self "Grab me"