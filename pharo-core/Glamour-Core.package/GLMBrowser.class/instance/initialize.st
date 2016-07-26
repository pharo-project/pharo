initialize-release
initialize
	super initialize.
	panes := OrderedCollection new.
	transmissions := OrderedCollection new.
	statusPane := nil.
	watcherPane := nil