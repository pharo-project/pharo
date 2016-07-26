morphic ui
repositorySelection
	"Return the index of the selected repository.
	May return 0, which means that no repository is selected"
	^ self repositories indexOf: self repository