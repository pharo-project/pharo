Class {
	#name : 'PBBootstrap',
	#superclass : 'Object',
	#instVars : [
		'architecture',
		'imageReference',
		'buildNumber',
		'originRepository',
		'ringEnvironment',
		'versionInfo',
		'repositoryLocation',
		'logger'
	],
	#category : 'PharoBootstrap',
	#package : 'PharoBootstrap'
}

{ #category : 'instance creation' }
PBBootstrap class >> for32Bits [

	^ self forArchitecture: '32'
]

{ #category : 'instance creation' }
PBBootstrap class >> for64Bits [

	^ self forArchitecture: '64'
]

{ #category : 'instance creation' }
PBBootstrap class >> forArchitecture: architecture [

	^ super new
		architecture: architecture;
		yourself
]

{ #category : 'instance creation' }
PBBootstrap class >> forArchitecture: architecture buildNumber: buildNumber versionInfo: versionInfo [
	
	^ super new
		architecture: architecture;
		versionInfo: versionInfo;
		imageName: 'bootstrap.image';
		yourself
]

{ #category : 'instance creation' }
PBBootstrap class >> fromCommandLine [
	| options architecture buildNumber versionInfo |
	options := CommandLineArguments new.
	architecture := options optionAt: 'ARCH' ifAbsent: [ '32' ].
	buildNumber := options optionAt: 'BUILD_NUMBER'
		ifPresent: [ :read | Number readFrom: read ifFail: [ -1 ] ]
		ifAbsent: [ -1 ].
	versionInfo := options optionAt: 'VERSION_INFO' ifAbsent: [ self error: 'Must provide VERSION_INFO in the form of a git describe --long' ].
	
	('    [+] Bootstrapping for a ' , architecture , '-bit architecture') traceCr.
	('    [+] Build number: ' , buildNumber asString) traceCr.
	('    [+] VersionInfo: ' , versionInfo) traceCr. 
	
	versionInfo := (PBVersionInfo fromLongGitDescribe: versionInfo)
		buildNumber: buildNumber;
		yourself.
	
	^ self forArchitecture: architecture buildNumber: buildNumber versionInfo: versionInfo
]

{ #category : 'instance creation' }
PBBootstrap class >> new [

	self error: 'Please use one of the factory methods #for32Bits or #for64Bits'
]

{ #category : 'instance creation' }
PBBootstrap class >> repositoryLocation [
	"Returns the location of the repository that we want to bootstrap, which is usually the working directory. Can be overridden if BOOTSTRAP_REPOSITORY is specified"
	^ (Smalltalk os environment
		at: 'BOOTSTRAP_REPOSITORY'
		ifAbsent: [ '.' ]) asFileReference
]

{ #category : 'package-names' }
PBBootstrap >> allPackagesForHermes [

	^ #BaselineOfPharoBootstrap asClass allPackageNames
]

{ #category : 'accessing' }
PBBootstrap >> architecture: aString [

	architecture := aString
]

{ #category : 'bootstrapping' }
PBBootstrap >> bootstrap [

	logger
		execute: [
			logger log: '[+] Preparing bootstrap'.
			self prepareBootstrap.

			logger log: '[+] Creating image on which bootstrapping will take place'.
			[ self createImage ]
				on: AssertionFailure
				do: [ :e | e resume ].

			logger log: '[+] Initializing image for bootstrapping'.
			self initializeImage ]
		logged: 'Bootstrap process'
]

{ #category : 'accessing' }
PBBootstrap >> bootstrapCacheDirectory [

	^ (Smalltalk os environment at: 'BOOTSTRAP_CACHE' ifAbsent: ['bootstrap-cache']) asFileReference ensureCreateDirectory
]

{ #category : 'accessing' }
PBBootstrap >> buildNumber [

	^ buildNumber
]

{ #category : 'accessing' }
PBBootstrap >> buildNumber: aBuildNumber [

	buildNumber := aBuildNumber
]

{ #category : 'bootstrapping' }
PBBootstrap >> createImage [

	| builder |
	builder := PBAbstractImageBuilder forArchitecture: architecture.
	builder logger: logger.
	builder versionInfo: versionInfo.
	builder imageFileReference: imageReference.
	builder systemDefinition: ringEnvironment.
	builder espellBackend instanceVariableMapping: (PBInstanceVariableMapping onEnvironment: ringEnvironment).
	builder bootstrap
]

{ #category : 'pre-requisites' }
PBBootstrap >> ensureBaselineOfPharoBootstrap [

	#( BaselineOfKernel BaselineOfCompiler BaselineOfTraits BaselineOfFileSystem BaselineOfMonticello BaselineOfPharoBootstrap BaselineOfMetacello ) do: [
		:baselineName | (self originRepository versionWithInfo: (self originRepository versionInfoFromVersionNamed: baselineName)) load ]
]

{ #category : 'preparation' }
PBBootstrap >> exportAllPackagesIntoMcz [

	self mczCache directory ensureDeleteAll; ensureCreateDirectory.
	self packagesToExportAsMCZ do: [ :packageName |
		self exportIntoMcz: packageName ].
]

{ #category : 'preparation' }
PBBootstrap >> exportIntoMcz: aPackageName [

	self mczCache storeVersion: (self originRepository loadVersionFromFileNamed: aPackageName).
]

{ #category : 'preparation' }
PBBootstrap >> exportKernelProtocols [
	"This method exports all protocols of the methots in the kernel in a single text file to be easily readed and imported from the 'in-midst of initialization' image."
		
	self exportProtocolsFor: self kernelPackageNames to: self bootstrapCacheDirectory / 'protocolsKernel.txt'.
	
	
]

{ #category : 'preparation' }
PBBootstrap >> exportProtocolsFor: packageNames to: protocolsFile [

	"This methods takes the packages from the (filetree) origin repository, reads all methods in these packages and export the their protocols in a single text file with the following format:
		[className]\tab[class is meta?]\tab[selector]\tab[protocol]."
		
	| originRepository |
	originRepository := self originRepository.
	protocolsFile ensureDelete.
	protocolsFile writeStreamDo: [:stream | | versions |
		versions := originRepository directory directories sorted.
		versions
			select: [ :file | packageNames includes: file basenameWithoutExtension ]
			thenDo: [:file | | version |
		    version := originRepository versionWithInfo: (originRepository versionInfoFromVersionNamed: file basenameWithoutExtension).
		    (version snapshot definitions select: #isMethodDefinition) do: [:def |
		      stream nextPutAll: def className asString; tab.
		      stream nextPutAll: def classIsMeta asString; tab.
		      stream nextPutAll: def selector asString; tab.
		      stream nextPutAll: def category asString; cr ]]]
]

{ #category : 'preparation' }
PBBootstrap >> generateHermesFiles [
	| oldPackage hePackage writer |
	self hermesPackageNames
		do: [ :packageName | 
			oldPackage := ringEnvironment ask packageNamed: packageName.
			hePackage := HERing2ToHermesBuilder new visitPackage: oldPackage.
			writer := HEBinaryReaderWriter new
				stream: (File openForWriteFileNamed: (self bootstrapCacheDirectory / packageName , 'hermes') fullName);
				yourself.
			hePackage writeInto: writer.
			writer flush.
			writer close]
]

{ #category : 'package-names' }
PBBootstrap >> hermesPackageNames [

	^ #BaselineOfPharoBootstrap asClass packagesToExportWithHermes
]

{ #category : 'accessing' }
PBBootstrap >> imageName: aString [ 
	
	imageReference := (self bootstrapCacheDirectory / aString) asFileReference
]

{ #category : 'initialization' }
PBBootstrap >> initialize [
	super initialize.
	logger := PBLogger new
]

{ #category : 'initialization' }
PBBootstrap >> initializeImage [
	
]

{ #category : 'package-names' }
PBBootstrap >> kernelPackageNames [

	^ #BaselineOfPharoBootstrap asClass kernelPackageNames
]

{ #category : 'accessing' }
PBBootstrap >> mczCache [
	
	^ MCDirectoryRepository new directory: (self bootstrapCacheDirectory / 'pharo-local' / 'package-cache') ensureCreateDirectory
]

{ #category : 'accessing' }
PBBootstrap >> originRepository [
	| repositoryReference |
	repositoryReference := self repositoryLocation / 'src'.

	^ originRepository
		ifNil: [ originRepository := TonelRepository new
				directory: repositoryReference;
				yourself]
]

{ #category : 'package-names' }
PBBootstrap >> packagesToExportAsMCZ [

	^ #BaselineOfPharoBootstrap asClass packagesToExportAsMCZ
]

{ #category : 'preparation' }
PBBootstrap >> prepareBootstrap [

	self
		ensureBaselineOfPharoBootstrap;
		exportKernelProtocols;
		exportAllPackagesIntoMcz;
		prepareEnvironmentForHermes;
		generateHermesFiles;
		prepareEnvironmentForExport
]

{ #category : 'preparation' }
PBBootstrap >> prepareEnvironmentForExport [
	| allPackages |

	allPackages := ringEnvironment packages collect: #name.

	(allPackages reject: [ :x | #BaselineOfPharoBootstrap asClass kernelPackageNames includes: x ])
		do: [ :x | 
			| p |
			p := ringEnvironment ask packageNamed: x.
			p extensionMethods do: [ :e | e methodClass removeLocalMethod: e ].
			p definedBehaviors
				do: [ :e | 
					ringEnvironment removeBehavior: e.
					ringEnvironment removeBehavior: e classSide
					].
			ringEnvironment removePackage: p].

	ringEnvironment cleanGlobalVariables.
	ringEnvironment addGlobalsNamed: #(#Smalltalk #Transcript #Undeclared #Display #TextConstants  #Sensor #Processor).
	ringEnvironment clean

]

{ #category : 'preparation' }
PBBootstrap >> prepareEnvironmentForHermes [
	ringEnvironment := self originRepository asRing2EnvironmentWith: self allPackagesForHermes.
	ringEnvironment fixProtoObjectClassSuperclass.
	ringEnvironment addGlobalsNamed: #(Smalltalk Transcript FileStream MacRomanTextConverter Processor Display Sensor UTF8TextConverter Undeclared TextConstants).
	ringEnvironment clean
]

{ #category : 'accessing' }
PBBootstrap >> repositoryLocation [

	^ repositoryLocation ifNil: [ self class repositoryLocation ]
]

{ #category : 'accessing' }
PBBootstrap >> repositoryLocation: aFileReference [

	repositoryLocation := aFileReference
]

{ #category : 'accessing' }
PBBootstrap >> versionInfo [
	^ versionInfo
]

{ #category : 'accessing' }
PBBootstrap >> versionInfo: anObject [
	versionInfo := anObject
]
