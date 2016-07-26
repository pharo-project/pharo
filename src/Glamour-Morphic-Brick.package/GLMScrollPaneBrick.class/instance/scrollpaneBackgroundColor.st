brick-properties
scrollpaneBackgroundColor

	^ self valueOfProperty: #brickScrollpaneBackgroundColor ifAbsent: [ self themer scrollpaneBackgroundColor ]