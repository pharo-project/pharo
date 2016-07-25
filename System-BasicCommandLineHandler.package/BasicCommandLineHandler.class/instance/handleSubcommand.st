activation
handleSubcommand
	"check if there is an active subcommand"
	
	| handlers |
	
	handlers := self selectHandlers.
	
	"If there is no handler, use myself"
	handlers ifEmpty: [ ^ self ].
		
	^self activateSubCommand: handlers first .
	