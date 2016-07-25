*Morphic-Base
acceptDroppingMorph: dropped event: evt
	"The supplied morph, known to be acceptable to the receiver, is now to be assimilated; the precipitating event is supplied"

	| aMorph |
	aMorph := self morphToDropFrom: dropped.	"Add the given morph to this world and start stepping it if it wants to be."
	self addMorphFront: aMorph.
	(aMorph fullBounds intersects: self viewBox)
		ifFalse: [ 
			self inform: 'Dropped morph not in bounds'.
			aMorph position: self bounds center ].
	aMorph
		submorphsDo: [ :m | 
			(m isKindOf: HaloMorph)
				ifTrue: [ m delete ] ].
	self bringTopmostsToFront