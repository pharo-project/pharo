context
downInContext: aContext
	"move down the context stack to the previous (enclosing) context"

	self flag: 'This does not take into account (bypasses) filtering'.
	^ aContext sender. 