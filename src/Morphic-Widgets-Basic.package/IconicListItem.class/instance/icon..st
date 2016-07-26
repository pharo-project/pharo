accessing
icon: anObject

	icon ifNotNil: [ self removeMorph: icon ].
	
	icon := anObject asAlphaImageMorph.
	anObject ifNotNil: [ "since the text can be removed" 
		self addMorphFront: icon.
		self adjustLayoutBounds]
	