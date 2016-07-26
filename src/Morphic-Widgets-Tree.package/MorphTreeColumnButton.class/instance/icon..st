accessing
icon: anIcon
	
	icon ifNotNil: [ self removeMorph: icon ].
	
	anIcon ifNotNil: [
		icon := anIcon.
		self addMorphFront: anIcon ]