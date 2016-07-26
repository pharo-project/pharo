brick-properties
rubricBorderColor

	^ self valueOfProperty: #brickRubricBorderColor ifAbsent: [ self themer rubricBorderColor ]