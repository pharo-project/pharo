events
popUpFor: aMorph event: evt 
	"This message is sent by morphs that explicitly request the halo on a button click. Note: anEvent is in aMorphs coordinate frame."

	| hand anEvent |
	self flag: #workAround.	"We should really have some event/hand here..."
	anEvent := evt isNil 
				ifTrue: 
					[hand := aMorph world ifNotNil: [:w | w activeHand].
					hand ifNil: [hand := aMorph world primaryHand]. 
					hand lastEvent transformedBy: (aMorph transformedFrom: nil)]
				ifFalse: 
					[hand := evt hand.
					evt].
	self target: aMorph.
	hand halo: self.
	hand world addMorphFront: self.
	positionOffset := anEvent position 
				- (aMorph point: aMorph position in: owner).
	self startStepping.
	