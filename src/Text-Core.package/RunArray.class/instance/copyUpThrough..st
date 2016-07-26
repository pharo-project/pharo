copying
copyUpThrough: anElement
	"Optimized"

	| newValues |
	newValues := values copyUpThrough: anElement.
	^ self class
		runs: (runs copyFrom: 1 to: newValues size)
		values: newValues