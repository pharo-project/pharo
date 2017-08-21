as yet unclassified
gitRepositoryUrl: url location: location subdirectory: subdirectory

	repository := IceRepositoryCreator new 
		remote: (IceRemote url: url);
		location: location asFileReference;
		subdirectory: subdirectory;
		createRepository.
		
	commitHash := (OSEnvironment current getEnv: 'TRAVIS_COMMIT')
		ifNil: [[repository headCommit commitId] ifError: ['UNKNOWN']].

	Transcript cr; show: 'Commit hash: '; show: commitHash asString; cr.