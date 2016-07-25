examples - synchronize
exampleGlamourSynchronizerTestForUpdate
	| repository |
	repository := IceRepository origin: 'git@github.com:npasserini/pharo-git-test.git'.
	repository checkoutBranch: 'master~2'.
	(repository packageNamed: 'Pharo-Git-Test') load.
	repository checkoutBranch: 'master'.
	IceGlamourSynchronizer synchronize: repository.
