drag and drop
dragPassengersFor: item inMorph: dragSource 

	| transferType object |
	
	(dragSource isKindOf: self class listClass) ifFalse: [^ nil ].
	transferType := self dragTransferTypeForMorph: dragSource.
	object := item originalObject.
	transferType == #getCategoryItem: ifTrue: [ ^ self selectedCategories ifEmpty: [ { object } ] ].
	^ nil