tests - basic
testBasicCommandWithArgument
	| command | 
	
	command := self newCommand
					command: '/bin/ls';
					arguments: { Smalltalk image imagePath. }.
	
	command runAndWait