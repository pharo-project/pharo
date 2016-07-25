initialization
reset
	"Clear the damage list."

	invalidRects := OrderedCollection new: 15.
	totalRepaint := false
