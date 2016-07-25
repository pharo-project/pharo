brick-properties
rubricTextColor

	^ self valueOfProperty: #brickRubricTextColor ifAbsent: [ self themer rubricTextColor ]