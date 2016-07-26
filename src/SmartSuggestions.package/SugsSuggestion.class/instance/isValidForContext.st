execution
isValidForContext
	^ isValidBlock ifNil:[true] ifNotNil:[isValidBlock value: context].