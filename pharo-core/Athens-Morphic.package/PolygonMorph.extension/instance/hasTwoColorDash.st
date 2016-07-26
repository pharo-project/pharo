*Athens-Morphic
hasTwoColorDash
	" return true if the dash spec has another color"

	^ self dashedBorder notNil and: [ self dashedBorder size > 2 ]