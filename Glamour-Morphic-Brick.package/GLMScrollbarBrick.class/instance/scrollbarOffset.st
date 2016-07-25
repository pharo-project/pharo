brick-properties
scrollbarOffset

	^ self valueOfProperty: #brickScrollbarOffset ifAbsent: [ self themer scrollbarOffset ]