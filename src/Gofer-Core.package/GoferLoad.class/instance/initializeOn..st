initialization
initializeOn: aGofer
	super initializeOn: aGofer.
	aGofer resolved
		do: [ :each | self addResolved: each ]
		displayingProgress: 'Loading Versions'