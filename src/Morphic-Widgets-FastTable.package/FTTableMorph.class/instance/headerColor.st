accessing colors
headerColor
	^ headerColor ifNil: [ self class defaultHeaderColor ]