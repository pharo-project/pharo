accessing
morph: anObject
	
	morph ifNotNil: [ self removeMorph: morph ].
	morph := anObject.
	morph ifNotNil: [ "since the text can be removed" 
		self addMorphBack: morph.
		self adjustLayoutBounds]
	