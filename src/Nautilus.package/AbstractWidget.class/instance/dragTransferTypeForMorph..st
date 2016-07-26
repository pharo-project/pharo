drag and drop
dragTransferTypeForMorph: dragSource 

	^(dragSource isKindOf: self class listClass)
		ifTrue: [ dragSource getListElementSelector ]