as yet unclassified
gitRepositoryUrl: url location: location subdirectory: subdirectory

	repository := IceRepositoryCreator new 
		remote: (IceRemote url: url);
		location: location asFileReference;
		subdirectory: subdirectory;
		createRepository.
		
	"in some cases (like on Travis) the last commit hash resolution can fail"
	commitHash := [repository headCommit commitId] ifError: ['UNKNOWN'].
