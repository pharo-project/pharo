private
checkIsValid

	(handle isNil or: [ handle isValid not ]) ifTrue: [ self invalidHandle ].