copying
copyUpToLast: anElement
	"Optimized"

	| newValues |
	newValues := values copyUpToLast: anElement.
	^ self class
		runs: (runs copyFrom: 1 to: newValues size)
		values: newValues