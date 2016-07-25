drag and drop
wantsDroppedMorph: transferMorph event: evt inMorph: listMorph 
	| targetItem targetItemMorph |
	(transferMorph isKindOf: TransferMorph) ifFalse: [ ^ false ].
	transferMorph dragTransferType = self dragTransferType ifFalse: [ ^ false ].
	
	targetItemMorph := (listMorph scrollerSubMorphFromPoint: evt position).
	targetItemMorph ifNotNil: [
		targetItem := targetItemMorph complexContents item.	
		^ self glamourPresentation allow: transferMorph passenger droppedOnItem: targetItem ].
	self flag: 'we should ask the whole presentation if it wants to receive a dropped object'.
	^ false