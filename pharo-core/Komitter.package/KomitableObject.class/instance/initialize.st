initialize
initialize

	super initialize.
	author := Author fullName.
	entities := Dictionary new.
	unselectedEntities := Dictionary new.
	message := ''.
	timestamp := DateAndTime now.