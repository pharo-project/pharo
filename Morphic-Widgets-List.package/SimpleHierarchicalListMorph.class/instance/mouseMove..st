event handling
mouseMove: evt

	|aMorph|
	aMorph := self itemFromPoint: evt position.
	evt hand hasSubmorphs ifFalse: [
		(aMorph isNil or: [aMorph highlightedForMouseDown not])
			ifTrue: [scroller submorphsDo: [:m |
					m highlightedForMouseDown ifTrue: [m highlightForMouseDown: false]].
					aMorph ifNotNil: [aMorph highlightForMouseDown]]].
	(self dropEnabled and:[evt hand hasSubmorphs]) 
		ifFalse:[^super mouseMove: evt].
	potentialDropMorph ifNotNil:[
		(potentialDropMorph containsPoint: (potentialDropMorph point: evt position from: self))
			ifTrue:[^self].
	].
	self mouseLeaveDragging: evt.
	(self containsPoint: evt position) 
		ifTrue:[self mouseEnterDragging: evt].