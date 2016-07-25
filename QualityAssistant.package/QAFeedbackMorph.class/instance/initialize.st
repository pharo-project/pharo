initialization
initialize
	super initialize.
	
	self
		layoutPolicy: TableLayout new;
		listDirection: #leftToRight;
		cellPositioning: #center;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		cellInset: 3;
		layoutInset: 3@0;
		cornerStyle: #rounded;
		color: Color transparent;
		borderColor: (Color black alpha: 0.1).
		
	self
		addMorphBack: (text := 'Helpful?' asMorph);
		addMorphBack: (self initializePositiveFeedback);
		addMorphBack: (self initializeNegativeFeedback)