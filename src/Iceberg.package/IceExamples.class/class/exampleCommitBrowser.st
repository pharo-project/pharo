examples - commit
exampleCommitBrowser
	| repository changeSet |
	repository := IceRepository origin: 'git@github.com:npasserini/iceberg.git'.
	changeSet := IceDiff forRepository: repository.
	IceCommitBrowser new
		changeSet: changeSet;
		openWithSpec.
