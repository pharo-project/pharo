initialization
initialize
	super initialize.
	type := 'Pharo'.
	major := 0.
	minor := 0.	 	
	suffix := ''.
	build := -1. "means no build number given, as we also accept a build number 0"
	commitHash := ''.
	date := Date today.