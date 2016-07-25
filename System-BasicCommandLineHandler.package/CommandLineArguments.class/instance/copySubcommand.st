copying
copySubcommand
	"return a new copy of this CommandLine without the first arguments"
	^ self class withArguments: arguments allButFirst