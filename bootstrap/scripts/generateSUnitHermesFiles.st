| toExport repository packageNames environment |

Transcript nextPutAll: '[Hermes] - Loading BaselineOfPharoBootstrap'; cr.

repository := TonelRepository new
	directory: 'src' asFileReference;
	yourself.
(repository versionWithInfo: (repository versionInfoFromVersionNamed: 'Metacello-PharoExtensions')) snapshot install.
(repository versionWithInfo: (repository versionInfoFromVersionNamed: 'BaselineOfPharoBootstrap')) snapshot install.

Transcript nextPutAll: '[Hermes] - Generating Environment from repository'; cr.

toExport := #BaselineOfPharoBootstrap asClass sUnitPackageNames.
	
packageNames := #BaselineOfPharoBootstrap asClass kernelPackageNames, toExport.
environment := repository asRing2EnvironmentWith: packageNames.		

environment fixProtoObjectClassSuperclass.
environment addGlobalsNamed: #(Undeclared Smalltalk UTF8TextConverter MacRomanTextConverter FileStream ChangeSet Character Processor SystemOrganization).

environment clean.

toExport do: [ :packageName |
	| oldPackage hePackage writer |
	oldPackage := environment ask packageNamed: packageName.
	hePackage := HERing2ToHermesBuilder new visitPackage: (oldPackage).
	writer := HEBinaryReaderWriter new
			stream: (((Smalltalk os environment at: 'BOOTSTRAP_CACHE' ifAbsent: ['bootstrap-cache']) asFileReference / packageName, 'hermes'));
			yourself.
	hePackage writeInto: writer.
	writer flush.
	writer close. ].

Transcript nextPutAll: '[Hermes] - Done'; cr.
