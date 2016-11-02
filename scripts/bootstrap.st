"Added compatibility method: RPackageOrganizer>>addMethod:
This is because Iceberg is meant to be compatible only with Pharo 6.0 and the bootstrap works on Pharo5.0"

! !RPackageOrganizer methodsFor: 'private' stamp: 'CamilloBruni 10/17/2012 11:31'!
addMethod: method

	| rPackage protocol |

	protocol := method protocol ifNil: [ Error signal: 'Should not happen' ].

	rPackage := (self hasPackageForProtocol: protocol inClass: method methodClass)
		ifTrue: [ self packageForProtocol: protocol inClass: method methodClass ]
		ifFalse: [ self registerPackage: (self packageClass named: (protocol copyWithout: $*)) ].

    rPackage ifNotNil: [
		rPackage addMethod: method]! !

[ Metacello new
	baseline: 'Iceberg';
	repository: 'github://npasserini/iceberg';
	load: 'development'.

"After installing Iceberg, we should set the default backend.
In this way, ci jobs will not be asked for credentials"
IceRepository defaultBackendType: IceLibgitLocalRepository.

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
