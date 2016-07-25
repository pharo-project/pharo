copying
copyUpTo: anElement 
	"Optimized"

	| newValues |
	newValues := values copyUpTo: anElement.
	^ self class
		runs: (runs copyFrom: 1 to: newValues size)
		values: newValues