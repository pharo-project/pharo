action
optionalOperations
	^ pattern isEmptyOrNil
		ifTrue: [ #() ]
		ifFalse: [ nil ]