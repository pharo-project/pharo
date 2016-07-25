examples - commit
exampleCommitBrowserTest
	| repository changeSet |
	repository := IceRepository origin: 'git@github.com:npasserini/pharo-git-test.git'.
	changeSet := IceDiff forRepository: repository.
	IceCommitBrowser new
		changeSet: changeSet;
		openWithSpec.
