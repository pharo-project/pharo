initialization
initialize

	super initialize.

	toolbar := TabBarMorph new.
	processes := OrderedCollection new.
	self registerBarActions.
	self registerShortcuts.
	
	container := Morph new
		color: Smalltalk ui theme class lightBaseColor;
		changeProportionalLayout;
		yourself.
		
	contentsWrapper := Morph new
		color: Smalltalk ui theme class lightBaseColor;
		changeProportionalLayout;
		yourself.
		
	self changeProportionalLayout.
	self addMorphs