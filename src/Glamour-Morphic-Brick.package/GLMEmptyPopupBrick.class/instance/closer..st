accessing
closer: aPopupCloser
	"Change current popup closer to a given one"
	
	self removeCloserListener.
	closer := aPopupCloser.
	self addCloserListener