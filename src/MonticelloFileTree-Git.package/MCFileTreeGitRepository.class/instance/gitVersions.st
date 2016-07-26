git querying
gitVersions
	"Query git for all the commits in the repository"

	"This is used to establish an ancestry chain package per package."

	^ ReadStream on: (self gitCommand: #('log' '--format=commit %H %P') in: directory)