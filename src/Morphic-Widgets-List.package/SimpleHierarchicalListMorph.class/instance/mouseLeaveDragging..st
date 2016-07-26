event handling
mouseLeaveDragging: anEvent
	(self dropEnabled and:[anEvent hand hasSubmorphs]) ifFalse: ["no d&d"
		^ super mouseLeaveDragging: anEvent].
	self resetPotentialDropMorph.
	anEvent hand releaseMouseFocus: self.
	"above is ugly but necessary for now"
