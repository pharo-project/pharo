events-processing
doubleClick: anEvent 
	| targetMorph |
	targetMorph := self scrollerSubMorphFromPoint: anEvent position.
	(self listManager doubleClick:  anEvent on: targetMorph)
		ifFalse: [super doubleClick: anEvent]
