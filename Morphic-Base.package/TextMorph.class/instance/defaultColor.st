initialization
defaultColor

	^ defaultColor ifNil: [ self theme textColor ]