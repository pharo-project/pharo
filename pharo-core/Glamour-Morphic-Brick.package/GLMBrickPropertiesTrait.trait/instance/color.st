brick-properties
color

	^ self valueOfProperty: #brickBackgroundColor ifAbsent: [ self themer backgroundColor ]