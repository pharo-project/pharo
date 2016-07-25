drag and drop
dragPassengersFor: item inMorph: dragSource 

	| transferType object |
	(dragSource isKindOf: self class listClass) ifFalse: [^ nil ].
	transferType := self dragTransferTypeForMorph: dragSource.
	transferType == #getMethodItem: ifFalse: [ ^ nil ].
	object := item originalObject.

	^ self selectedMethods
		ifEmpty: [
			 (self getMethods includes: object)
				ifTrue: [ { object } ]]