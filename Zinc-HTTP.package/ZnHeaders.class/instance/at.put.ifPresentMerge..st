accessing
at: headerName put: value ifPresentMerge: binaryBlock
	"Store value under headerName, when there is an existing entry,
	stored the result of evaluating binaryBlock with old and new value"
	
	| normalizedKey existingValue newValue |
	normalizedKey := self normalizeHeaderKey: headerName.
	existingValue := self headers at: normalizedKey ifAbsent: [ nil ].
	newValue := existingValue isNil
		ifTrue: [ value ] 
		ifFalse: [ binaryBlock value: existingValue value: value ].
	^ self headers at: normalizedKey put: newValue 
		

