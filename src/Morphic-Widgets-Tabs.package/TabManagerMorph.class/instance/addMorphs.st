private
addMorphs

	self 
		addMorph: toolbar
		fullFrame: (LayoutFrame identity
			bottomFraction: 0;
			bottomOffset: TabMorph defaultHeight + 2;
			yourself).
			
	self 
		addMorph: container
		fullFrame: (LayoutFrame identity
			topOffset: TabMorph defaultHeight + 2;
			yourself).
			
	container
		addMorph: contentsWrapper
		fullFrame:
			(LayoutFrame identity
				topOffset: 2;
				rightOffset: -1;
				bottomOffset: -1;
				leftOffset: 2;
				yourself).