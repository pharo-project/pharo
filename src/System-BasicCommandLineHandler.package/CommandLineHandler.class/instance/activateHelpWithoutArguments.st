activation
activateHelpWithoutArguments
	"Default help implementation, running #help if the there is no argument or a single one which is --help "
	((self hasOption: 'help') or: [ self arguments isEmpty ])
		ifTrue: [ 
			self help.
			^ true ].
	^ false