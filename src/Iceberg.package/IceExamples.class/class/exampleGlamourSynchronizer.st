examples - synchronize
exampleGlamourSynchronizer
	| repository |
	repository := IceRepository origin: 'git@github.com:npasserini/iceberg.git'.
	IceGlamourSynchronizer synchronize: repository.
