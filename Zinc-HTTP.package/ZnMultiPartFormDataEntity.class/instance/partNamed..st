accessing
partNamed: fieldName
	^ self partNamed: fieldName ifNone: [ self error: 'Cannot find part named ', fieldName asString ]