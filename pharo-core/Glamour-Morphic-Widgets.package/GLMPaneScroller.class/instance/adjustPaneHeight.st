layout
adjustPaneHeight
	"This gets called after the scrollbar has been shown or hidden, to move the bottom
	of the panes to the right place."
	
	transform bounds: self innerBounds.
	transform submorphsDo: [:m | m bounds: (m bounds withHeight: self paneHeight)]