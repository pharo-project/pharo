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
	repository: 'github://npasserini/iceberg:dev-0.4';
	load.

Metacello new
	baseline: 'PharoBootstrapProcess';
	repository: 'filetree://bootstrap';
	load.

options := CommandLineArguments default.
architecture := options optionAt: 'ARCH' ifAbsent: [ '32' ].

"Configure logging"
(CustomStringStreamBeacon with: FileStream stdout)
	startFor: PBStepStartedSignal, PBStepFinishedSignal.

(CustomStringStreamBeacon with: 'bootstrap.log' asFileReference writeStream)
	startFor: PBStepStartedSignal, PBStepFinishedSignal, PBSubStepFinishedSignal.

(PBBootstrap forArchitecture: architecture)
	prepareBootstrap;
	createImage

] on: Warning do: #resume
