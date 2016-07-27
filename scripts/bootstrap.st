[ Metacello new
	baseline: 'Iceberg';
	repository: 'github://npasserini/iceberg';
	load: 'development'.

Metacello new
	baseline: 'PharoBootstrapProcess';
	repository: 'github://guillep/PharoBootstrap';
	load ] on: Warning do: #resume.

options := CommandLineArguments default.
architecture := options optionAt: 'ARCH' ifAbsent: [ '32' ].

(#'PBBootstrapSpur50', architecture ,'bit') asSymbol asClass new
	gitRepositoryUrl: 'git@github.com:guillep/pharo-core.git'
	location: '.'
	subdirectory: 'src';
	espellBackend: ((#EPSimulatorBackend asClass) perform: (#'for', architecture, 'Bit') asSymbol) forBootstrap;
	bootstrap.

Smalltalk snapshot: false andQuit: true.