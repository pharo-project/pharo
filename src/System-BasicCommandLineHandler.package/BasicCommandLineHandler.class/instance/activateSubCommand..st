activation
activateSubCommand: aCommandLinehandler
	[ aCommandLinehandler activateWith: commandLine ] on: Exit do: [ :exit |
		^ self handleExit: exit for: aCommandLinehandler ].
	"the return value of this method is used to check if the subcommand was successfull"
	^ aCommandLinehandler