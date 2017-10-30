argument validation
checkSubCommand: aString 
	(self availableSubCommands includes: aString)
		ifFalse: [
			self exitFailure: ('{1} subcommand does not exists' format: { aString }) ]