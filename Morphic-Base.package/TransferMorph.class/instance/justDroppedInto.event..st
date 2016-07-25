dropping/grabbing
justDroppedInto: targetMorph event: anEvent 
	"If only world wants this TransferMorph, treat it as unaccepted (see also >>delete)."
	| formerPosition |
	formerPosition := self formerPosition.
	super justDroppedInto: targetMorph event: anEvent.
	self formerPosition: formerPosition.
	accepted := targetMorph ~= self world.
	self animationForMoveSuccess: accepted.
	accepted ifTrue: [
		self dropNotifyRecipient ifNotNil: [
			self dropNotifyRecipient dropAcceptedMorph: self from: targetMorph ]].
	self delete