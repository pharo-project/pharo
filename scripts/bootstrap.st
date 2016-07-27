[ Metacello new
	baseline: 'Iceberg';
	repository: 'github://npasserini/iceberg';
	load: 'development'.

Metacello new
	baseline: 'PharoBootstrapProcess';
	repository: 'github://guillep/PharoBootstrap';
	load ] on: Warning do: #resume.
	
#PBBootstrapSpur5032bit asClass new
	gitRepositoryUrl: 'git@github.com:guillep/pharo-core.git'
	location: '.'
	subdirectory: 'src';
	espellBackend: #EPSimulatorBackend asClass for32Bit forBootstrap;
	bootstrap.

Smalltalk snapshot: false andQuit: true.