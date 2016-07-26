event handling
acceptDroppingMorph: dropped event: evt
	"The supplied morph, known to be acceptable to the receiver, is now to be assimilated; the precipitating event is supplied"

	| aMorph |
	aMorph := self morphToDropFrom: dropped.
	super acceptDroppingMorph: aMorph event: evt.
	aMorph
		submorphsDo: [ :m | 
			(m isKindOf: HaloMorph)
				ifTrue: [ m delete ] ].
	self bringTopmostsToFront