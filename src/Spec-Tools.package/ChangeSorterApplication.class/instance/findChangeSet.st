menu - change set
findChangeSet

	| set |
	set := self model findChangeSetIn: self changeSets.
	changesListPresenter setSelectedItem: set.