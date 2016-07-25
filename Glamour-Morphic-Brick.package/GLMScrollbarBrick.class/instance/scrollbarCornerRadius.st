brick-properties
scrollbarCornerRadius

	^ self valueOfProperty: #brickScrollbarCornerRadius ifAbsent: [ self themer scrollbarCornerRadius ]
	