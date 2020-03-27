Class {
	#name : #FLPlatform,
	#superclass : #Object,
	#classVars : [
		'Current'
	],
	#category : #'Fuel-Platform-Core'
}

{ #category : #'private-extensions' }
FLPlatform class >> addExtensions [
	self extensionPragmas do: [ :pragma |
		(FLExtensionWrapper fromPragma: pragma) install ]
]

{ #category : #operations }
FLPlatform class >> addHacks [
	self current
		useAuthor: 'Fuel'
		during: [ self basicAddHacks ]
]

{ #category : #'private-hacks' }
FLPlatform class >> basicAddHacks [
	self
		fixMetacello;
		addExtensions
]

{ #category : #accessing }
FLPlatform class >> current [
	^ Current ifNil: [ Current := self detectPlatform ]
]

{ #category : #private }
FLPlatform class >> detectPlatform [
	| matches mostSpecific |
	matches := self allSubclasses 
		select: [ :class | class isResponsibleForCurrentPlatform ].
	mostSpecific := matches first.
	matches do: [ :class |
		class allSubclasses size < mostSpecific allSubclasses size ifTrue: [
			mostSpecific := class ] ].
	
	^ mostSpecific new
]

{ #category : #accessing }
FLPlatform class >> extensionCategoryName [
	^ 'FuelPlatformExtensions'
]

{ #category : #'private-extensions' }
FLPlatform class >> extensionPragmas [
	| pragmas |
	pragmas := Dictionary new.
	#(
		flExtensionOf:
		flExtensionOf:selector:
		flExtensionOf:selector:isSource:
		flExtensionOf:isSource:) do: [ :selector |
			| stop |
			stop := false.
			self class withAllSuperclassesDo: [ :class |
				stop ifFalse: [
					Pragma withPragmasIn: class do:  [ :pragma |
						(pragma selector = selector and: [
							"don't collect overridden methods"
							(pragmas includesKey: pragma methodSelector) not ]) ifTrue: [
								pragmas
									at: pragma methodSelector
									put: pragma ] ].
						FLPlatform class = class ifTrue: [ stop := true ] ] ] ].
			
	^ pragmas values
]

{ #category : #accessing }
FLPlatform class >> extensionProtocolName [
	^ '*', self extensionCategoryName
]

{ #category : #'private-hacks-metacello' }
FLPlatform class >> fixMetacello [
]

{ #category : #accessing }
FLPlatform class >> hacksCategoryName [
	^ 'FuelPlatformHacks'
]

{ #category : #testing }
FLPlatform class >> isResponsibleForCurrentPlatform [
	^ self subclassResponsibility
]

{ #category : #'private-convenience' }
FLPlatform class >> removeModifications [
	((Smalltalk globals at: #(Smalltalk globals at: #PackageInfo)) named: self extensionProtocolName) extensionMethods do: [ :methodReference |
		methodReference actualClass removeSelector: methodReference selector ].
	((Smalltalk globals at: #(Smalltalk globals at: #PackageInfo)) named: self hacksCategoryName) in: [ :hacks |
		hacks classes do: [ :classOrTrait |
			classOrTrait removeFromSystem ].
		(Smalltalk globals at: #PackageOrganizer) default  unregisterPackage: hacks ].
	SystemOrganizer default removeCategory: self hacksCategoryName
]

{ #category : #initialization }
FLPlatform class >> reset [
	Current := nil
]

{ #category : #'accessing-system' }
FLPlatform >> absolutePathOf: aString [
	^ self subclassResponsibility
]

{ #category : #'accessing-compiler' }
FLPlatform >> addAndClassifySelector: aSymbol inClass: aClass withMethod: aCompiledMethod inProtocol: aString [
	self subclassResponsibility
]

{ #category : #operations }
FLPlatform >> addHacks [
	self class addHacks
]

{ #category : #'accessing-tests' }
FLPlatform >> classFactoryForTestCaseClass [
	^ ClassFactoryForTestCase
]

{ #category : #'accessing-files' }
FLPlatform >> deleteFileNamed: aFilename [
	self subclassResponsibility
]

{ #category : #'accessing-files' }
FLPlatform >> fileNamed: aFilename readStreamDo: aBlock [
	^ self subclassResponsibility
]

{ #category : #'accessing-files' }
FLPlatform >> fileNamed: aFilename writeStreamDo: aBlock [
	^ self subclassResponsibility
]

{ #category : #'accessing-system' }
FLPlatform >> imageName [
	^ self subclassResponsibility
]

{ #category : #testing }
FLPlatform >> isBigEndian [
	^ self subclassResponsibility
]

{ #category : #testing }
FLPlatform >> isPharo [
	^ false
]

{ #category : #testing }
FLPlatform >> isSpur [
	^ true
]

{ #category : #testing }
FLPlatform >> isSqueak [
	^ false
]

{ #category : #'accessing-compiler' }
FLPlatform >> renamedMethodCopyOf: aMethod selector: aSymbol [
	^ self subclassResponsibility
]

{ #category : #'accessing-tests' }
FLPlatform >> setTestTimeLimitFor: aTest to: aDuration [
	self subclassResponsibility
]

{ #category : #'accessing-compiler' }
FLPlatform >> silentlyAddAndClassifySelector: aSymbol inClass: aClass withMethod: aCompiledMethod inProtocol: aString [
	self subclassResponsibility
]

{ #category : #'accessing-system' }
FLPlatform >> useAuthor: aString during: aBlock [
	self subclassResponsibility
]

{ #category : #accessing }
FLPlatform >> versionNumber [
	^ ('\d+' asRegex matchesIn: self className) first asInteger
]
