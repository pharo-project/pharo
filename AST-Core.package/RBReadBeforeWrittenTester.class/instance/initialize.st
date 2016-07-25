initialization
initialize
	super initialize.
	scopeStack := OrderedCollection with: Dictionary new.
	read := Set new.
	checkNewTemps := true