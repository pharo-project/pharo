bootstrap
createImage

	self flag: #fixArguments.
	
	(PBImageBuilderSpur50 forArchitecture: architecture)
		gitRepositoryUrl: 'UNUSED' location: 'UNUSED' subdirectory: 'UNUSED';
	 	imageFileReference: imageReference;  
		bootstrap.