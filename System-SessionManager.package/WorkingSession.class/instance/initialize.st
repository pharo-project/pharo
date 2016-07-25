initialization
initialize
	super initialize.
	deferredStartupActions := OrderedCollection new.
	creationTime := DateAndTime now.
	id := UUID new.