| toExport ftRepo environment rfienvironment |

Transcript nextPutAll: '[Hermes] - Loading BaselineOfPharoBootstrap'; cr.
ftRepo := MCFileTreeRepository new directory: 'src' asFileReference.
(ftRepo versionWithInfo: (ftRepo versionInfoFromVersionNamed: 'BaselineOfPharoBootstrap')) snapshot install.

Transcript nextPutAll: '[Hermes] - Generating Environment from repository'; cr.
toExport := #BaselineOfPharoBootstrap asClass sUnitPackageNames.
environment := Dictionary new.
(#BaselineOfPharoBootstrap asClass kernelPackageNames),
	toExport do: [ :name | | version |
		version := FiMonticelloWriter new 
			written: (ftRepo versionWithInfo: (ftRepo versionInfoFromVersionNamed: name )) snapshot
			name: name.
		environment at: name ifAbsentPut: version.
].

rfienvironment := environment values asFiSet asRFiEnvironment.

Transcript nextPutAll: '[Hermes] - Creating Globals'; cr.

"Required by OpalCompiler"
rfienvironment globalByName at: #Smalltalk put: nil.

"Required by CodeExport"
rfienvironment globalByName at: #UTF8TextConverter put: nil.
rfienvironment globalByName at: #MacRomanTextConverter put: nil.
rfienvironment globalByName at: #FileStream put: nil.
rfienvironment globalByName at: #FileSystem put: nil.

"Required by CodeImport"
rfienvironment globalByName at: #ChangeSet put: nil.


toExport do: [ :name | | hePackage writer |
	Transcript nextPutAll: ('[Hermes] - Exporting Package ' , name); cr.

	hePackage := HERingToHermesBuilder new visitPackage: (rfienvironment packages detect: [ :p | p name = name ]).
	writer := HEBinaryReaderWriter new
			stream: (File openForWriteFileNamed: 'bootstrap-cache/' , name, '.hermes');
			yourself.
	hePackage writeInto: writer.
	writer flush.
	writer close.
].

Transcript nextPutAll: '[Hermes] - Done'; cr.