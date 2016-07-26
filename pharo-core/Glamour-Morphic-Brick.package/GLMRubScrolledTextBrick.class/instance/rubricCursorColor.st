brick-properties
rubricCursorColor

	^ self valueOfProperty: #brickRubricCursorColor ifAbsent: [ self themer rubricCursorColor ]