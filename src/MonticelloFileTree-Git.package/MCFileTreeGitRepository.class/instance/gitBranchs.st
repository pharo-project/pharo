git querying
gitBranchs
	"Query git for all the branches in the repository"

	"This is used to determine the current branch."

	| s |
	s := (self gitCommand: #('branch') in: directory) substrings.
	^ s