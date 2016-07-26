private
addMorphs

	self 
		addMorph: toolbar
		fullFrame: (LayoutFrame identity
			bottomFraction: 0;
			bottomOffset: 27;
			yourself).
			
	self 
		addMorph: container
		fullFrame: (LayoutFrame identity
			topOffset: 27;
			yourself).
			
	container
		addMorph: contentsWrapper
		fullFrame:
			(LayoutFrame identity
				topOffset: 10;
				rightOffset: -1;
				bottomOffset: -1;
				leftOffset: 2;
				yourself).