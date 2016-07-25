initialization
initialize

	super initialize.

	tabs := OrderedCollection new.
	selectionHistory := OrderedCollection new.
	menuButton := self createMenuButton.
	numberOfSelectedTabs := 1.
	
	self changeProportionalLayout.
	self addMenuButton.
	