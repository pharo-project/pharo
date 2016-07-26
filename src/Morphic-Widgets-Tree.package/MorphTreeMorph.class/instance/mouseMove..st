events-processing
mouseMove: evt

	| targetMorph |
	self enabled ifFalse: [ ^self ].
	targetMorph := self scrollerSubMorphFromPoint: evt position.
	evt hand hasSubmorphs 
		ifFalse: [(self innerBounds containsPoint: evt position)
			ifTrue: [self listManager mouseMove: evt on: targetMorph]].
		
	(self dropEnabled and: [evt hand hasSubmorphs]) 
		ifFalse:[^super mouseMove: evt].
		
	potentialDropMorph 
		ifNotNil:[
			(potentialDropMorph containsPoint: (potentialDropMorph point: evt position from: self))
				ifTrue:[^self]].
			
	self mouseLeaveDragging: evt.
	
	(self containsPoint: evt position) 
		ifTrue: [self mouseEnterDragging: evt].