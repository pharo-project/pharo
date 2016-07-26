private
initializeJob: aJob

	super initialize.
	
	lock := Semaphore forMutualExclusion.
	job := aJob.
	hasResult := false.
	lastRefresh := 0.
	
	self
		color: Color transparent;
		layoutPolicy: TableLayout new;
		listDirection: #topToBottom;
		cellPositioning: #leftCenter;
		listCentering: #center;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap.
		
	labelMorph := StringMorph 
		contents: job title 
		font: StandardFonts defaultFont.
	bar := JobProgressBarMorph new.
	bar 
		on: #mouseUp send: #debug to: self;
		hResizing: #spaceFill.
	self updateLayout.