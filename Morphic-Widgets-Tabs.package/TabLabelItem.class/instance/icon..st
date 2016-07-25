accessing
icon: anObject

	icon ifNotNil: [ self removeMorph: icon ].
	
	icon := anObject.
	icon ifNotNil: [ 	self addMorphFront: icon ]