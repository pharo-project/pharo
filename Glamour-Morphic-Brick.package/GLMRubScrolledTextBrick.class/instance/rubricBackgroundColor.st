brick-properties
rubricBackgroundColor

	^ self valueOfProperty: #brickRubricBackgroundColor ifAbsent: [ self themer rubricBackgroundColor ]