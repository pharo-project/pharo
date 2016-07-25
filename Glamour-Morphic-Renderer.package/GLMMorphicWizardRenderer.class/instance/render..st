rendering
render: aBrowser
	container := GLMMorphic containerMorph.
	container
		changeTableLayout;
		listDirection: #leftToRight;
		vResizing: #spaceFill;
		hResizing: #spaceFill.
	scrollPane := GeneralScrollPane new
		changeScrollerTableLayout;
		scrollTarget: container;
		yourself.
	self registerAnnouncementsFor: aBrowser.  
	self addMorphFromObject: (aBrowser stepsAndPanes at: aBrowser currentStep) last toContainer: container .
	^ scrollPane