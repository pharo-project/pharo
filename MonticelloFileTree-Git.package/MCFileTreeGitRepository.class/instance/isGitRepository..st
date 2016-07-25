testing
isGitRepository: aDirectory
	"Check that we have a git repository"

	^ (self
		gitCommand: #('rev-parse' '--is-inside-work-tree')
		in: aDirectory) substrings first = 'true'