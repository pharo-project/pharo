private
prepareSubcommand: commandLineArguments

	"strip the subcommand name from the arguments"
	commandLineArguments withFirstArgument: [ :arg|
		arg = self commandName 
			ifTrue: [ ^ commandLineArguments copySubcommand ]].
	
	"not a subcommand hence we keep the same args"
	^ commandLineArguments