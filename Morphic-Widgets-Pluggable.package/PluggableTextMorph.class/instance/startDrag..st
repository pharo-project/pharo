drag and drop
startDrag: event
	| aTransferMorph itemMorph passenger |
	
	self dragEnabled ifFalse: [ ^ self ].

	itemMorph := self selectedContents asMorph.
	
	passenger := self model dragPassengerFor: itemMorph inMorph: self.
	passenger
		ifNotNil: [
			aTransferMorph := self model transferFor: passenger from: self.
				"Ask the draggedMorph otherwise the transferMorph has not yet its bounds"
			aTransferMorph align: aTransferMorph draggedMorph center with: event position.
			aTransferMorph
				dragTransferType: (self model dragTransferTypeForMorph: self).
			event hand grabMorph: aTransferMorph].
	event hand releaseMouseFocus: self