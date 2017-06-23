instance creation
backend: aBackend
	"This should be the only way to create a repository. Usually, this creator is called on 
	 IceRepositoryCreator>>#createRepository."
	^ self new 
		backend: aBackend;
		yourself