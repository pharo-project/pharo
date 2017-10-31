accessing
subCommand
	| firstArg |
	firstArg := self argumentAt: 1.
	self checkSubCommand: firstArg.
	^ firstArg