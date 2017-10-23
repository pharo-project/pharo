menu - change set
remove
	"Completely destroy my change set.  Check if it's OK first"

	self okToChange ifFalse: [^ self].
	self model removeChangeSet: self selectedChangeSet prompting: true.
	self updateChangesList.
	changesListPresenter resetSelection