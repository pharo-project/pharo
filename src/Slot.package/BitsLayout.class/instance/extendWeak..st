extending
extendWeak: aLayoutClassScope 
	IncompatibleLayoutConflict new
		layout: self;
		subType: #weak;
		signal