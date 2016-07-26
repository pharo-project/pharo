private
initializeLabel: aString start: startNumber end: endNumber
	
	lock := Semaphore forMutualExclusion.
	hasResult := false.
	lastRefresh := 0.
	
	self
		color: Color transparent;
		layoutPolicy: TableLayout new;
		listDirection: #topToBottom;
		cellPositioning: #center;
		cellInset: 5;
		listCentering: #center;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
		
	labelMorph := StringMorph contents: aString font: StandardFonts windowTitleFont.
	self addMorphBack: labelMorph.

	bar := ProgressBarMorph from: startNumber to: endNumber.
	self addMorphBack: bar.