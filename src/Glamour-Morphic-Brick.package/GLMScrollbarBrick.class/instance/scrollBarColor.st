brick-properties
scrollBarColor

	^ self valueOfProperty: #brickScrollbarColor ifAbsent: [ self themer scrollbarColor ]