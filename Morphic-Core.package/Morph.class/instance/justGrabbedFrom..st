dropping/grabbing
justGrabbedFrom: formerOwner
	"The receiver was just grabbed from its former owner and is now attached to the hand. By default, we pass this message on if we're a renderer."
	(self isRenderer and:[self hasSubmorphs]) 
		ifTrue:[self firstSubmorph justGrabbedFrom: formerOwner].