event handling
startDrag: anEvent
	| aTransferMorph |
	self dragEnabled ifFalse: [^ self].
	(anEvent hand hasSubmorphs)
		ifTrue: [^ self].
	oldColor 
		ifNotNil: [
			self fillStyle: oldColor.
			oldColor := nil].
	aTransferMorph := self model transferFor: self model from: self.
	aTransferMorph align: aTransferMorph draggedMorph center with: anEvent position.
	anEvent hand grabMorph: aTransferMorph.
	anEvent hand releaseMouseFocus: self