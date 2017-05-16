as yet unclassified
gitRepositoryUrl: url location: location subdirectory: subdirectory

	repository := IceRepositoryCreator new 
		remote: (IceRemote url: 'git@github.com:pharo-project/pharo.git');
		location: '.' asFileReference;
		subdirectory:'src';
		createRepository.