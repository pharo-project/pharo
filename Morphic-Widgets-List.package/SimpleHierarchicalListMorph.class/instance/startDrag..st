event handling
startDrag: anEvent 
	| aTransferMorph itemMorph passenger |
	self dragEnabled
		ifTrue: [itemMorph := scroller submorphs
						detect: [:any | any highlightedForMouseDown]
						ifNone: []].
	(itemMorph isNil
			or: [anEvent hand hasSubmorphs])
		ifTrue: [^ self].
	itemMorph highlightForMouseDown: false.
	itemMorph ~= self selectedMorph
		ifTrue: [self setSelectedMorph: itemMorph].
	passenger := self model dragPassengerFor: itemMorph inMorph: self.
	passenger
		ifNotNil: [
			aTransferMorph := self model transferFor: passenger from: self.
			aTransferMorph align: aTransferMorph draggedMorph center with: anEvent position.
			aTransferMorph
				dragTransferType: (self model dragTransferTypeForMorph: self).
			anEvent hand grabMorph: aTransferMorph].
	anEvent hand releaseMouseFocus: self