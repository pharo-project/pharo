accessing
state: newState
	"Change the image and invalidate the rect."

	newState == state ifTrue: [^ self].
	state := newState.
	self
		adoptPaneColor: self paneColor;
		changed