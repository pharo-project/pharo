initialization
initialize
	super initialize.
	type := 'Pharo'.
	major := 0.
	minor := 0.	 	
	suffix := ''.
	build := self class invalidBuildNumber.
	commitHash := ''.
	date := Date today.