brick-properties
textColor

	^ self valueOfProperty: #brickTextColor ifAbsent: [ self themer textColor ]