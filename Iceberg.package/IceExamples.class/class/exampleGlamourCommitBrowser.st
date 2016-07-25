examples - commit
exampleGlamourCommitBrowser
	| repository changeSet |
	repository := IceRepository origin: 'git@github.com:npasserini/iceberg.git'.
	changeSet := IceDiff forRepository: repository.
	IceGlamourCommitBrowser openOn: changeSet.