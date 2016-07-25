initialization
initialize
	super initialize.
	
	self
		emptyBrick: self newEmptyText;
		maxHeight: 350;
		height: [ :b | b band height ];
		width: 250