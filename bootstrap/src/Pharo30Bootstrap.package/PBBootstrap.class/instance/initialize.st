initialization
initialize
	"Initialize the logging"

	| repo |
	(CustomStringStreamBeacon with: FileStream stdout) startFor: PBStepStartedSignal , PBStepFinishedSignal.

	repo := IceRepositoryCreator new
		location: self repositoryLocation;
		subdirectory: 'src';
		createRepository.

	versionFetcher := PBRepositoryVersionReader new
		repository: repo;
		buildNumber: buildNumber;
		major: 7;
		minor: 0;
		yourself