examples - synchronize
exampleSynchronizer
	| repository |
	repository := IceRepository origin: 'git@github.com:npasserini/iceberg.git'.
	IceSynchronizer new
		repository: repository;
		openWithSpec.
