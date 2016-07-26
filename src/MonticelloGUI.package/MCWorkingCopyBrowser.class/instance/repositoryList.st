morphic ui
repositoryList
	"Return the list of repository names. Only String are returned"
	| repositories |
	repositories := self repositories.

	^ repositories collect: [:ea | ea description]