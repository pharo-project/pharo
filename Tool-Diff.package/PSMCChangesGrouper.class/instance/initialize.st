initialization
initialize

	super initialize.
	classChanges := OrderedCollection new.
	nonClassChanges := OrderedCollection new.
	classes := Set new.