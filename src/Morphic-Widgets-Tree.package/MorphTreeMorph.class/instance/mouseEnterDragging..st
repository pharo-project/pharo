events-processing
mouseEnterDragging: evt
	| aMorph |
	(evt hand hasSubmorphs and: [self dropEnabled]) 
		ifFalse: [^super mouseEnterDragging: evt].
	(self wantsDroppedMorph: evt hand firstSubmorph event: evt)
		ifTrue: [
			aMorph := self scrollerSubMorphFromPoint: evt position.
			aMorph ifNotNil:[self potentialDropMorph: aMorph].
			evt hand newMouseFocus: self.
			"above is ugly but necessary for now"].