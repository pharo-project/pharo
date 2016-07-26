initialization
initialize
	super initialize.
	ports := OrderedCollection new.
	presentations := GLMCompositePresentation new pane: self; yourself