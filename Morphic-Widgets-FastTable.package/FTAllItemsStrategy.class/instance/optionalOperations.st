action
optionalOperations
	pattern isEmptyOrNil
		ifTrue: [ self unexpandAllChildOf: dataSource rootItem.
			^ #() ]
		ifFalse: [ ^ nil ]