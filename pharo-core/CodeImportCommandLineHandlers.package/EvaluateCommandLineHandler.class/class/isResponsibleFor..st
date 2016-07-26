handler selection
isResponsibleFor: commandLineArguments
	"directly handle top-level -e and --evaluate options"
	commandLineArguments withFirstArgument: [ :arg| 
		(#('-e' '--evaluate') includes: arg)
			ifTrue: [ ^ true ]].
	
	^ commandLineArguments includesSubCommand: self commandName