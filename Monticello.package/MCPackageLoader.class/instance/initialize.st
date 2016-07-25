initialization
initialize
	super initialize.
	additions := OrderedCollection new.
	removals := OrderedCollection new.
	obsoletions := Dictionary new.
	methodAdditions := OrderedCollection new. 
