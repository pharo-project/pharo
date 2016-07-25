initialization
initialize

	super initialize.
	lock := Semaphore forMutualExclusion.
	lastRefresh := 0.
	self
		setDefaultParameters;
		setProperty: #morphicLayerNumber toValue: self morphicLayerNumber;
		layoutPolicy: TableLayout new;
		listDirection: #topToBottom;
		cellPositioning: #topCenter;
		cellInset: 5;
		listCentering: #center;
		hResizing: #rigid;
		vResizing: #shrinkWrap;
		layoutInset: 10@6;
		minWidth: 150