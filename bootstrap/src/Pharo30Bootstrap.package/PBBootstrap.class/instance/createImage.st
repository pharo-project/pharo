bootstrap
createImage

	(PBImageBuilderSpur50 forArchitecture: architecture)
		gitRepositoryUrl: 'git@github.com:pharo-project/pharo.git' location: '.' subdirectory: 'src';
	 	imageFileReference: imageReference;  
		bootstrap.