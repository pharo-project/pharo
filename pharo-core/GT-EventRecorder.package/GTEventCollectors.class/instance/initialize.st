initialization
initialize
	super initialize.
	mutex := Mutex new.
	collectors := OrderedCollection new.