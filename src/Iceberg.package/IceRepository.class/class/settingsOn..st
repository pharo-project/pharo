settings
settingsOn: aBuilder 
	<systemsettings>
	(aBuilder pickOne: #'defaultBackendType')
		parent: #Iceberg;
		target: self;
		label: 'Backend type';
		description: 'Select your preferred way to access git repositories from Iceberg: the git command line (OSSubprocess) or the libgit2 library';
		domainValues: 
			((TIceRepositoryBackend traitUsers collect: [:class | class description -> class name ]) 
				ifEmpty: { 'No backend' -> nil }).
	
	(aBuilder setting: #shareRepositoriesBetweenImages)
		parent: #Iceberg;
		target: self;
		label: 'Share repositories between images';
		description: 'Check if you want to have your git repositories cloned by default in a single location that is shared between all your Pharo images';
		with: [
			(aBuilder setting: #sharedRepositoriesLocationString)
				target: self;
				type: #Directory;
				label: 'Location for shared repositories';
				description: 'A directory where you have your git repositories'.
		].
		