accessing
state: newState
	"Change the image and invalidate the rect."

	newState == state ifTrue: [^ self].
	state := newState.
	self invalidRect: bounds.	"All three images must be the same size"