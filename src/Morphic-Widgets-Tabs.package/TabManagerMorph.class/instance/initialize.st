initialization
initialize

	super initialize.

	toolbar := TabBarMorph new.
	processes := OrderedCollection new.
	self registerBarActions.
	self registerShortcuts.
	
	container := Morph new
		color: self containerColor;
		changeProportionalLayout;
		yourself.
		
	contentsWrapper := Morph new
		color: self containerColor;
		changeProportionalLayout;
		yourself.
		
	self changeProportionalLayout.
	self addMorphs