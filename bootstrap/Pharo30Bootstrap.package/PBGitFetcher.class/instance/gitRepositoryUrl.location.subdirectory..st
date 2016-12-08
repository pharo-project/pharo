as yet unclassified
gitRepositoryUrl: url location: location subdirectory: subdirectory

	repository := (IceRepository
		origin: 'git@github.com:guillep/pharo-core.git'
		location: '.' asFileReference )
			subdirectory: 'src'