initialization
initialize
	super initialize.
	
	allTags := OrderedCollection new.
	selectedTags := OrderedCollection new.
	defaultTag := #custom