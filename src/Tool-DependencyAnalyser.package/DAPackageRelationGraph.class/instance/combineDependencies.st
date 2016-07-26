actions
combineDependencies
	"Combine all dependencies with the same source and target into one composite
	dependency."
	self packages 
		do: [ :each | self combineDependencies: each ]
		displayingProgress: 'Combining Dependencies'