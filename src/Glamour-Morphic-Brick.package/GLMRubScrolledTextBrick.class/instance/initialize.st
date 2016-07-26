initialization
initialize
	super initialize.
	
	self rubric: self newRubricMorph.
	self withoutBackground.
	self
		hSpaceFill;
		vSpaceFill