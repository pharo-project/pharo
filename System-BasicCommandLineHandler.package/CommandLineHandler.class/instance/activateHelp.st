activation
activateHelp
	"Default help implementation, running #help if the only argument is --help "
	((self hasOption: 'help') and: [ self arguments size = 1 ])
		ifTrue: [ 
			self help.
			^ true ].
	^ false