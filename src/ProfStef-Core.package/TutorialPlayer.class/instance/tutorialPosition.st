accessing
tutorialPosition 
	^ tutorialPosition  ifNil: [
		self rewind.
		tutorialPosition.
	].
	