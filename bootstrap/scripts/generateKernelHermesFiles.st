| toExport repository packageNames environment repositoryReference |

Transcript nextPutAll: '[Hermes] - Loading BaselineOfPharoBootstrap'; cr.

repositoryReference := (Smalltalk os environment at: 'BOOTSTRAP_REPOSITORY' ifAbsent: ['.']) asFileReference / 'src'.

repository := TonelRepository new
	directory: repositoryReference;
	yourself.
(repository versionWithInfo: (repository versionInfoFromVersionNamed: 'Metacello-PharoExtensions')) snapshot install.
(repository versionWithInfo: (repository versionInfoFromVersionNamed: 'BaselineOfPharoBootstrap')) snapshot install.

Transcript nextPutAll: '[Hermes] - Generating Environment from repository'; cr.

toExport := #BaselineOfPharoBootstrap asClass compilerPackageNames, {'Hermes-Extensions'}.
	
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
			stream: (File openForWriteFileNamed: ((Smalltalk os environment at: 'BOOTSTRAP_CACHE' ifAbsent: ['bootstrap-cache']) asFileReference / packageName, 'hermes') fullName) ;
			yourself.
	hePackage writeInto: writer.
	writer flush.
	writer close. ].

Transcript nextPutAll: '[Hermes] - Done'; cr.
