Class {
	#name : #FLPharoPlatform,
	#superclass : #FLPlatform,
	#category : #'Fuel-Platform-Pharo-Core'
}

{ #category : #'private-extensions-znbufferedreadstream' }
FLPharoPlatform class >> fuelNextWordsInto: aWordObject [
	<flExtensionOf: #ZnBufferedReadStream>
	"This method is the same as nextWordsInto: but the restoreEndianness is only done if needed"
	| byteSize source pos blt |
	byteSize := aWordObject byteSize.
	source := self next: byteSize.
	pos := 0.

	"Now use BitBlt to copy the bytes to the bitmap."
	blt := (BitBlt
				toForm: (Form new hackBits: aWordObject))
				sourceForm: (Form new hackBits: source).
	blt combinationRule: Form over. "store"
	blt sourceX: 0;
		 sourceY: pos // 4;
		 height: byteSize // 4;
		 width: 4.
	blt destX: 0;
		 destY: 0.
	blt copyBits.
	
	^ aWordObject
]

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

{ #category : #testing }
FLPharoPlatform >> hasStatefulTraits [
	^ true
]

{ #category : #'accessing-system' }
FLPharoPlatform >> imageName [
	^ Smalltalk image shortImageName
]

{ #category : #testing }
FLPharoPlatform >> isPharo [
	^ true
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
