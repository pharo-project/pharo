accessing selection
selectionChanged
	"Invalidate all the selection rectangles. 
	Make sure that any drop shadow is accounted for too."
	paragraph ifNil: [ ^ self ].
	self recomputeSelection.
