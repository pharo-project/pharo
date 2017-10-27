initialization
initialize 
	super initialize.
	self
		clipSubmorphs:true;
		layoutPolicy: FTRowLayout new.
		
	selectionColor := self defaultColor.
	over := false
	