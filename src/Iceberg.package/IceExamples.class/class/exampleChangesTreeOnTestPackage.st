examples - change tree
exampleChangesTreeOnTestPackage
	| repository changeSet |
	repository := IceRepository origin: 'git@github.com:npasserini/pharo-git-test.git'.
	changeSet := IceDiff forRepository: repository.
	IceChangesTreeView new
		model: changeSet;
		openWithSpec.
		
	Transcript flush.