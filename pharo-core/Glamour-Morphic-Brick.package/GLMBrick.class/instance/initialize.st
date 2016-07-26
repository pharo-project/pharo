initialization
initialize
	super initialize.
	color := Color transparent.
	self fillStyle: nil.
	submorphs := OrderedCollection new.
	
	self
		layoutProperties: GLMBrickLayoutProperties new;
		useHorizontalLinearLayout;
		renderer: GLMBrickRenderer new