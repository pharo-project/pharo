rendering
render: aBrowser
	container := GLMPaneScroller new
		name: 'scroller';
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself.
	container maxPanes: aBrowser fixedSizePanes.
	self registerAnnouncementsFor: aBrowser.  
	aBrowser panes
		do: [ :each | container pushPane: (self renderObject: each) ].
	^ container