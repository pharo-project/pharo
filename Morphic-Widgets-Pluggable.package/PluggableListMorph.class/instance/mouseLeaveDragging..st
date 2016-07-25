event handling
mouseLeaveDragging: anEvent
	"The mouse has left with a button down."
	
	(self dragEnabled or: [anEvent hand hasSubmorphs]) ifFalse: [
		self listMorph mouseDownRow: nil].
	(self dropEnabled and: [anEvent hand hasSubmorphs]) ifFalse: ["no d&d"
		^super mouseLeaveDragging: anEvent].
	self resetPotentialDropRow.
	anEvent hand releaseMouseFocus: self.
	"above is ugly but necessary for now"