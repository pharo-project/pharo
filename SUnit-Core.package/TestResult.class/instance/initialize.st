initialization
initialize
	super initialize.
	passed := OrderedCollection new.
	failures := Set new.
	errors := OrderedCollection new.
	skipped := OrderedCollection new.
	timeStamp := DateAndTime now