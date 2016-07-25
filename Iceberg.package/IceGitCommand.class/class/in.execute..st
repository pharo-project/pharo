execute
in: directory execute: commandDescription
	"See class comments for the specification of possible command descriptions"
	^ MCFileTreeGitRepository 
		runGitCommand: (self preprocess: commandDescription) 
		in: directory