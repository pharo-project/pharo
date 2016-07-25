internal
checkValue: aValue

	(aValue isKindOf: self targetClass)
		ifFalse: [ self error: 'Invalid value' ].