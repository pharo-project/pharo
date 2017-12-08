[
| options architecture repository packageNames environment fetcher buildNumber bootstrap repositoryReference specificationsPath usedClasses calledMethods |

options := CommandLineArguments default.
architecture := options optionAt: 'ARCH' ifAbsent: [ '32' ].
buildNumber := options optionAt: 'BUILD_NUMBER' ifAbsent: [ '1' ].

"Configure logging"
(CustomStringStreamBeacon with: FileStream stdout)
	startFor: PBStepStartedSignal, PBStepFinishedSignal.

FileStream stdout
	nextPutAll: ('[INFO] Building Image Arch ', architecture);
	cr.

bootstrap := PBBootstrap forArchitecture: architecture.
bootstrap buildNumber: buildNumber;
	prepareBootstrap.
	
repositoryReference := (Smalltalk os environment at: 'BOOTSTRAP_REPOSITORY' ifAbsent: ['.']) asFileReference.
	
repository := TonelRepository new
	directory: repositoryReference / 'src';
	yourself.

(repository versionWithInfo: (repository versionInfoFromVersionNamed: 'Metacello-PharoExtensions')) snapshot install.
(repository versionWithInfo: (repository versionInfoFromVersionNamed: 'BaselineOfPharoBootstrap')) snapshot install.

packageNames := (#BaselineOfPharoBootstrap asClass kernelPackageNames).
	
environment := repository asRing2EnvironmentWith: packageNames.	

((environment ask behaviorNamed: #String) methodNamed: #asSlotCollection) sourceCode: 
'asSlotCollection
	^(self substrings: '' '') collect: [ :substring | substring asSlot ]'.

((environment ask behaviorNamed: #SlotClassBuilder) methodNamed: #validateClassName) sourceCode: 
'validateClassName'.

((environment ask behaviorNamed: #SystemVersion) methodNamed: #initialize) sourceCode: 
'initialize
	super initialize'.		
	
((environment ask behaviorNamed: #HEInstaller) methodNamed: #installPackage:) sourceCode: 
'installPackage: aHEPackage

	| newTraits newClasses m |
		
	RPackageOrganizer default registerPackageNamed: aHEPackage packageName.

	newTraits := aHEPackage traits collect: [ :exportedTrait | 
		(self environment includesKey: exportedTrait traitName) 
			ifFalse: [ self buildTrait: exportedTrait ]
			ifTrue: [self environment at: exportedTrait traitName ]].

	aHEPackage traits with: newTraits do: [ :exportedTrait :newTrait | self installMethods: exportedTrait into: newTrait ].

	newClasses := aHEPackage classes
		collect: [ :exportedClass | 
			| aClass |
			Transcript nextPutAll: ''loading: ''; nextPutAll: exportedClass className; cr. 
			(self environment includesKey: exportedClass className) 
				ifTrue: [ 
					''skipping...'' logCr.
					aClass := self environment at: exportedClass className. ]	 
				ifFalse: [ 
					aClass := self build: exportedClass.
					self installClass: aClass in: exportedClass category. ].
			aClass ].

	aHEPackage classes with: newClasses do: [ :exportedClass :newClass | self installMethods: exportedClass into: newClass ].

	aHEPackage extensionMethods do: [ :e | self installExtensionMethod: e ].
	#Done logCr.
'.	
	
((environment ask behaviorNamed: #HEInstaller) methodNamed: #rebuildMethod:into:) sourceCode: 
'rebuildMethod: aMethod into: aClass
	| newMethod literalSpace extendedEnvironment |

	Transcript nextPutAll: ''install method: ''; 
		nextPutAll: aMethod name; cr;
		nextPutAll: aClass name; 
		nextPutAll: ''>>#'';
		nextPutAll: aMethod name; cr.
	
	(aClass includesSelector: aMethod name ) ifTrue: [ 
		Transcript nextPutAll: ''                skipping...''; cr.
		^ self ].

	extendedEnvironment := HEExtendedEnvironment new.
	extendedEnvironment inner: environment.
	extendedEnvironment newClass: aClass.
	extendedEnvironment newSelector: aMethod name.

	newMethod := CompiledMethod newMethod: aMethod bytecode size header: (aMethod headerFor: extendedEnvironment).

	extendedEnvironment newMethod: newMethod.

	aMethod literals
		doWithIndex:
			[ :literal :idx | newMethod literalAt: idx put: (literal asLiteralIn: extendedEnvironment) ].

	newMethod classBinding: aClass binding.
	literalSpace := (aMethod literals size + 1) * Smalltalk wordSize.

	aMethod bytecode doWithIndex: [ :e :idx | newMethod at: idx + literalSpace put: e ].

	aClass
		addAndClassifySelector: aMethod name
		withMethod: newMethod
		inProtocol: aMethod protocol
		notifying: nil
'.	


specificationsPath := repositoryReference / 'bootstrap' / 'specifications'.

usedClasses := ((FileStream readOnlyFileNamed: specificationsPath / 'hermes-classes.txt') contentsOfEntireFile lines collect: #asSymbol) asIdentitySet.	
calledMethods := ((FileStream readOnlyFileNamed: specificationsPath / 'hermes-methods.txt') contentsOfEntireFile lines collect: #asSymbol) asIdentitySet.
environment removeAllButBehaviorsNamed: usedClasses.
environment removeAllButMethodsNamed: calledMethods.	

environment fixProtoObjectClassSuperclass.
environment addGlobalsNamed: #(#Smalltalk #SourceFiles #Transcript #Undeclared #Display #TextConstants  #Sensor #Processor #SystemOrganization).
environment clean.

fetcher := PBRing2GitFetcher new.
fetcher gitRepositoryUrl: 'git@github.com:pharo-project/pharo.git' location: repositoryReference subdirectory: 'src'.
fetcher buildNumber: buildNumber.
	
environment bootstrapWith: fetcher architecture: architecture to: bootstrap imageReference.	

] on: Warning do: #resume
