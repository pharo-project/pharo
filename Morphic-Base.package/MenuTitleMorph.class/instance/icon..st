construction
icon: aForm

	icon ifNotNil: [ icon delete ].
	icon := aForm asMorph.

	closeBox ifNotNil: [ ^self addMorph: icon after: closeBox  ].

	self addMorphFront: icon