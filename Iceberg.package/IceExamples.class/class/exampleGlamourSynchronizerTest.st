examples - synchronize
exampleGlamourSynchronizerTest
	| repository |
	repository := IceRepository origin: 'git@github.com:npasserini/pharo-git-test.git'.
	IceGlamourSynchronizer synchronize: repository.
