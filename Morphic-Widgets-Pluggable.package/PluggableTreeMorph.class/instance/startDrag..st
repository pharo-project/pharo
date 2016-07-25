morphic
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
		
	passenger := dragItemSelector
		ifNil: [ self model
			dragPassengerFor: itemMorph withoutListWrapper 
			inMorph: self ]
		ifNotNil: [
			self model 
				perform: dragItemSelector 
				withEnoughArguments: { itemMorph withoutListWrapper. self } ].
			
	passenger
		ifNotNil: [aTransferMorph := self model transferFor: passenger from: self.
			aTransferMorph dragTransferType: #dragTransfer.
			aTransferMorph align: aTransferMorph draggedMorph center with: anEvent position.
			anEvent hand grabMorph: aTransferMorph].
	anEvent hand releaseMouseFocus: self