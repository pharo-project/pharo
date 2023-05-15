Class {
	#name : #FLPharoPlatform,
	#superclass : #FLPlatform,
	#category : #'Fuel-Platform-Pharo-Core'
}

{ #category : #testing }
FLPharoPlatform class >> isResponsibleForCurrentPlatform [
	^ SystemVersion current version beginsWith: 'Pharo'
]

{ #category : #'accessing-system' }
FLPharoPlatform >> absolutePathOf: aString [
	^ aString asFileReference fullName
]

{ #category : #'accessing-compiler' }
FLPharoPlatform >> addAndClassifySelector: aSymbol inClass: aClass withMethod: aCompiledMethod inProtocol: aProtocol [

	aClass addAndClassifySelector: aSymbol withMethod: aCompiledMethod inProtocol: aProtocol
]

{ #category : #'accessing-files' }
FLPharoPlatform >> createTemporaryDirectoryPathNamed: aString [
	^ (FileLocator temp / aString) ensureCreateDirectory pathString
]

{ #category : #'accessing-files' }
FLPharoPlatform >> deleteAllInDirectoryPath: aPathString [
	aPathString asFileReference deleteAll
]

{ #category : #'accessing-files' }
FLPharoPlatform >> deleteFilePath: aFilename [
	((Smalltalk at: #File) named: aFilename) delete
]

{ #category : #'tests-testing' }
FLPharoPlatform >> expectedFailuresOf: aClass [
	| inherited |
	inherited := super expectedFailuresOf: aClass.
	(aClass includesBehavior: (Smalltalk at: #FLBasicSerializationTest)) ifTrue: [
		inherited addAll: #(
			"Global values are expected to be of type String. We'd need to modify string
			encoding to support String and WideString or treat the value as an arbitrary
			object and let the engine decide how to serialize."
			testWideStringGlobal
			testConsiderCustomWideSymbolGlobal
			testWideStringClassName) ].

	(FLVersion current major <= 5 and: [
		FLVersion current minor <= 1 ]) ifTrue: [
			(aClass includesBehavior: (Smalltalk at: #FLGlobalEnvironmentTest)) ifTrue: [
				inherited addAll: #(
					testCompiledMethodChanged) ].
			(aClass includesBehavior: (Smalltalk at: #FLBlockClosureSerializationTest)) ifTrue: [
				inherited addAll: #(
					testBlockClosureChangeDifferentBytecodes
					testBlockClosureChangeSameBytecodes
					testBlockClosureRemoved) ].
			(aClass includesBehavior: (Smalltalk at: #FLFullBlockClosureSerializationTest)) ifTrue: [
				inherited addAll: #(
					testBlockClosureChangeDifferentBytecodes) ] ].
		
	^ inherited
]

{ #category : #'accessing-files' }
FLPharoPlatform >> filePathExists: aPathString [
	^ aPathString asFileReference exists
]

{ #category : #testing }
FLPharoPlatform >> hasStatefulTraits [
	^ true
]

{ #category : #'accessing-system' }
FLPharoPlatform >> imageName [
	^ Smalltalk image shortImageName
]

{ #category : #testing }
FLPharoPlatform >> isBigEndian [
	^ (Smalltalk at: #EndianDetector) isBigEndian
]

{ #category : #'accessing-files' }
FLPharoPlatform >> joinPath: aPathString with: anotherPathString [
	^ (aPathString asFileReference / anotherPathString) pathString
]

{ #category : #'tests-compilation' }
FLPharoPlatform >> newAnonymousSubclassOf: aClass named: aString [
	^ aClass newAnonymousSubclass
]

{ #category : #'tests-compilation' }
FLPharoPlatform >> newAnonymousTraitNamed: aString [
	^ Smalltalk anonymousClassInstaller make: [ :builder |
		builder
			"The name is necessary to not break Pharo"
			name: aString;
			beTrait ]
]

{ #category : #'tests-compilation' }
FLPharoPlatform >> newSubclassOf: aClass named: className uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames: classVarsString poolDictionaries: poolNamesString environment: anEnvironment package: packageName [
	^ self class classInstaller make: [ :aBuilder | 
		aBuilder 
			environment: anEnvironment;
			name: className;
			superclass: aClass;
			traitComposition: aTraitComposition asTraitComposition;
			classTraitComposition: aTraitComposition asTraitComposition classComposition;
			slots: ivNamesString asSlotCollection;
			sharedVariablesFromString: classVarsString;
			sharedPools: poolNamesString;
			package: packageName ]
]

{ #category : #'tests-compilation' }
FLPharoPlatform >> newTraitNamed: aString uses: aTraitComposition instanceVariableNames: iVarString environment: anEnvironment package: packageName [
	^ self class classInstaller make: [ :aBuilder |
		aBuilder
			environment: anEnvironment;
			name: aString;
			slots: iVarString asSlotCollection;
			traitComposition: aTraitComposition asTraitComposition;
			package: packageName;
			beTrait ]
]

{ #category : #'accessing-files' }
FLPharoPlatform >> readStreamForFilePath: aString [
	^ ((Smalltalk at: #File) named: aString) readStream
		binary;
		yourself
]

{ #category : #'accessing-compiler' }
FLPharoPlatform >> renamedMethodCopyOf: aMethod selector: aSymbol [
	| newMethod |
	newMethod := aMethod ast
		selector: aSymbol
		yourself.
	"#compiledMethod answers a new instance"
	^ newMethod compiledMethod
]

{ #category : #'accessing-tests' }
FLPharoPlatform >> setTestTimeLimitFor: aTest to: aDuration [
	aTest timeLimit: aDuration
]

{ #category : #'accessing-compiler' }
FLPharoPlatform >> silentlyAddAndClassifySelector: aSymbol inClass: aClass withMethod: aCompiledMethod inProtocol: aString [
	aClass
		addSelector: aSymbol
		withMethod: aCompiledMethod.
	aClass organization
		classify: aSymbol
		under: aString
]

{ #category : #'accessing-system' }
FLPharoPlatform >> silentlyDo: aBlock [
	^ SystemAnnouncer uniqueInstance suspendAllWhile: aBlock
]

{ #category : #'accessing-system' }
FLPharoPlatform >> useAuthor: aString during: aBlock [
	(Smalltalk at: #Author)
		useAuthor: aString
		during: aBlock
]

{ #category : #'accessing-kernel' }
FLPharoPlatform >> withPragmasIn: aClass do: aBlock [
	aClass pragmasDo: aBlock
]

{ #category : #'accessing-files' }
FLPharoPlatform >> writeStreamForFilePath: aString [
	^ ((Smalltalk at: #File) named: aString) writeStream
		binary;
		yourself
]
