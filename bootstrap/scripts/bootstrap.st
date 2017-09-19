[
| options architecture repository packageNames environment fetcher buildNumber |

options := CommandLineArguments default.
architecture := options optionAt: 'ARCH' ifAbsent: [ '32' ].
buildNumber := options optionAt: 'BUILD_NUMBER' ifAbsent: [ 'NO_CI' ].

"Configure logging"
(CustomStringStreamBeacon with: FileStream stdout)
	startFor: PBStepStartedSignal, PBStepFinishedSignal.

FileStream stdout
	nextPutAll: ('[INFO] Building Image Arch ', architecture);
	cr.

(PBBootstrap forArchitecture: architecture)
	buildNumber: buildNumber;
	prepareBootstrap.
	
repository := MCFileTreeRepository new
	directory: './src' asFileReference;
	yourself.

(repository versionWithInfo: (repository versionInfoFromVersionNamed: 'BaselineOfPharoBootstrap')) snapshot install.

packageNames := (#BaselineOfPharoBootstrap asClass kernelPackageNames).
	
environment := repository asRing2EnvironmentWith: packageNames.		

environment fixProtoObjectClassSuperclass.
environment addGlobalsNamed: #(#Smalltalk #SourceFiles #Transcript #Undeclared #Display #TextConstants  #Sensor #Processor #SystemOrganization).
environment clean.

fetcher := PBRing2GitFetcher new.
fetcher gitRepositoryUrl: 'git@github.com:pharo-project/pharo.git' location: '.' subdirectory: 'src'.
fetcher buildNumber: buildNumber.
	
environment bootstrapWith: fetcher architecture: architecture.	

] on: Warning do: #resume
