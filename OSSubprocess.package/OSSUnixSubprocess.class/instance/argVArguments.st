argv building
argVArguments
	"Answers the whole list of argV including the command as the first argument and a
	null terminated."
	^ OrderedCollection new
		add: command;
		addAll: (arguments ifNil: [#()]);
		add: 0; "shell arguments are '\0' terminated"
		yourself