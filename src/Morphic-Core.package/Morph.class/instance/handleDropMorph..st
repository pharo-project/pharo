events-processing
handleDropMorph: anEvent
	"Handle a dropping morph."
	| aMorph localPt |
	aMorph := anEvent contents.
	"Do a symmetric check if both morphs like each other"
	((self wantsDroppedMorph: aMorph event: anEvent)	"I want her"
		and: [aMorph wantsToBeDroppedInto: self])		"she wants me"
		ifFalse: [^ self].
	anEvent wasHandled: true.
	"Transform the morph into the receiver's coordinate frame. This is currently incomplete since it only takes the offset into account where it really should take the entire transform."
	localPt := (self transformedFrom: anEvent hand world) "full transform down"
				globalPointToLocal: aMorph referencePosition.
	aMorph referencePosition: localPt.
	self acceptDroppingMorph: aMorph event: anEvent.
	aMorph justDroppedInto: self event: anEvent.
