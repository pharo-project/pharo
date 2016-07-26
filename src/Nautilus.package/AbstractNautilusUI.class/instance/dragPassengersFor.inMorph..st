drag and drop
dragPassengersFor: item inMorph: dragSource
	| transferType object |
	object := item originalObject.
	(dragSource isKindOf: AbstractWidget listClass)
		ifFalse: [ ^ nil ].
	transferType := self dragTransferTypeForMorph: dragSource.
	transferType == #getCategoryItem:
		ifTrue: [ ^ self selectedCategories ifEmpty: [ {object} ] ].
	transferType == #getClassItem:
		ifTrue: [ ^ self selectedClasses ifEmpty: [ {object} ] ].
	transferType == #getMethodItem:
		ifFalse: [ ^ nil ].
	^ self selectedMethods
		ifEmpty:
			[ 
			(self getMethods includes: object)
				ifTrue: [ {object} ]
				ifFalse: [ nil ] ]