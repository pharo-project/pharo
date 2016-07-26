initialization
initialize
	super initialize.
	
	self
		emptyBrick: self newEmptyText;
		maxHeight: 300;
		height: [ :b | b band height ];
		width: 300