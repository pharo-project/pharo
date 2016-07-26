change reporting
invalidRect: aRectangle from: aMorph
	| damageRect |
	aRectangle hasPositiveExtent ifFalse: [ ^self ].
	damageRect := aRectangle.
	aMorph == self ifFalse:[
		"Clip to receiver's clipping bounds if the damage came from a child"
		self clipSubmorphs 
			ifTrue:[damageRect := aRectangle intersect: self clippingBounds ifNone: [ ^ self ]]].
	owner ifNotNil: [owner invalidRect: damageRect from: self].