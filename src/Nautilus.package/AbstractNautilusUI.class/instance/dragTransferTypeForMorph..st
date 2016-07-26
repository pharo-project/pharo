drag and drop
dragTransferTypeForMorph: dragSource 

	^(dragSource isKindOf: AbstractWidget listClass)
		ifTrue: [ dragSource getListElementSelector ]