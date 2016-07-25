initialization
textColor
	"Answer my default text color."
	^ self 
		valueOfProperty: #textColor 
		ifAbsent: [ self theme textColor ]
