events-processing
startDrag: anEvent 
	| aTransferMorph itemMorph passenger |
	self dragEnabled
		ifTrue: [itemMorph := self allNodeMorphs
						detect: [:any | any highlightedForMouseDown]
						ifNone: []].
	(itemMorph isNil
			or: [anEvent hand hasSubmorphs])
		ifTrue: [^ self].
	itemMorph highlightForMouseDown: false.
	
	(self listManager selectedMorphList includes: itemMorph)
		ifFalse: [self listManager setSelectedMorph: itemMorph].
		
	passenger := self model dragPassengerFor: itemMorph inMorph: self.
	passenger
		ifNotNil: [
			aTransferMorph := self model transferFor: passenger from: self.
				"Ask the draggedMorph otherwise the transferMorph has not yet its bounds"
			aTransferMorph align: aTransferMorph draggedMorph center with: anEvent position.
			aTransferMorph
				dragTransferType: (self model dragTransferTypeForMorph: self).
			anEvent hand grabMorph: aTransferMorph].
	anEvent hand releaseMouseFocus: self