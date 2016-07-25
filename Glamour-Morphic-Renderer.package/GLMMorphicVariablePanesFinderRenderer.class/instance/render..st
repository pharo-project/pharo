rendering
render: aBrowser
	container := GLMMorphic containerMorph.
	container 
		changeTableLayout;
		listDirection: #leftToRight;
		vResizing: #spaceFill;
		hResizing: #shrinkWrap.
	scrollPane := GeneralScrollPane new
		changeScrollerTableLayout;
		scrollTarget: container;
		yourself.
	self registerAnnouncementsFor: aBrowser.  
	aBrowser panes do: [ :each | self addMorphFromObject: each toContainer: container ].
	^ scrollPane