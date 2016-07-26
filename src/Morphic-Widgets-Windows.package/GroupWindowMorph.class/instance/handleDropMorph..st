events-processing
handleDropMorph: anEvent
	"Handle a dropping morph."
	
	| aMorph |
	aMorph := anEvent contents.
	"Ignore whether the dropping morph wants to be dropped, just whether the receiver wants it"
	(self wantsDroppedMorph: aMorph event: anEvent)
		ifFalse: [^ self].
	anEvent wasHandled: true.
	self acceptDroppingMorph: aMorph event: anEvent.
	aMorph justDroppedInto: self event: anEvent
