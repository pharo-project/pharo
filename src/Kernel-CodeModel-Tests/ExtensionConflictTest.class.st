Class {
	#name : 'ExtensionConflictTest',
	#superclass : 'TestCase',
	#instVars : [
		'pointClass',
		'extensionOnPoint',
		'environment',
		'definerPackage',
		'userPackage',
		'objectClass',
		'coloredPointClass',
		'stringClass',
		'nicerStringClass',
		'userClass',
		'extensionOnObject',
		'extensionOnColoredPoint'
	],
	#category : 'Kernel-CodeModel-Tests-Extensions',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Extensions'
}

{ #category : 'running' }
ExtensionConflictTest >> setUp [

	| extendedPackage |
	super setUp.
	
	environment := SystemEnvironment new.
	extendedPackage := Package named: 'Extended'.
	definerPackage := Package named: 'Definer'.
	userPackage := Package named: 'User'.

	objectClass := (Object << #MyObject
		installingEnvironment: environment;
		package: userPackage) install.
	
	pointClass := (Object << #MyPoint
		superclass: objectClass;
		installingEnvironment: environment;
		package: userPackage) install.

	coloredPointClass := (Object << #MyColoredPoint
		superclass: pointClass;
		installingEnvironment: environment;
		package: userPackage) install.
	
	"String will have no extensions. It's here just for polymorphism.
	It will thus contain aliases only"
	stringClass := (Object << #MyString
		superclass: objectClass;
		installingEnvironment: environment;
		package: userPackage) install.
	
	nicerStringClass := (Object << #MyNicerString
		superclass: stringClass;
		installingEnvironment: environment;
		package: userPackage) install.

	userClass := (Object << #MyUserClass
		superclass: objectClass;
		installingEnvironment: environment;
		package: userPackage) install.
	
	extensionOnObject := (Extension << #ExtensionOfObject
		extendedClass: objectClass;
		installingEnvironment: environment;
		package: definerPackage) install.

	extensionOnPoint := (Extension << #ExtensionOfPoint
		extendedClass: pointClass;
		installingEnvironment: environment;
		package: definerPackage) install.

	extensionOnColoredPoint := (Extension << #ExtensionOfColoredPoint
		extendedClass: coloredPointClass;
		installingEnvironment: environment;
		package: definerPackage) install.
]

{ #category : 'running' }
ExtensionConflictTest >> tearDown [
	"Make sure we did not pollute the global dictionaries"

	self deny: (Point methodDictionary keys anySatisfy: [ :k | k isKindOf: ScopedSelector ]).
	self deny: (Object methodDictionary keys anySatisfy: [ :k | k isKindOf: ScopedSelector ]).
	self deny: (Integer methodDictionary keys anySatisfy: [ :k | k isKindOf: ScopedSelector ]).
	
	environment allClassesAndTraits do: [ :e | e removeFromSystem ].
	
	super tearDown
]

{ #category : 'tests' }
ExtensionConflictTest >> test01ExtensionConflictsWithPreExistingNormalMethod01NormalMethodWorksAsExpected [

	pointClass compile: 'extension ^ 17'.
	extensionOnPoint compile: 'extension ^ 73'.

	self assert: pointClass new extension equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test01ExtensionConflictsWithPreExistingNormalMethod02ExtensionThrowsException [

	| scopedSelector |
	pointClass compile: 'extension ^ 17'.
	scopedSelector := extensionOnPoint compile: 'extension ^ 73'.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: { extensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test01ExtensionConflictsWithPreExistingNormalMethod03SubclassesAreStillAliased [

	| scopedSelector |
	pointClass compile: 'extension ^ 17'.
	coloredPointClass compile: 'extension ^ 42'.
	scopedSelector := extensionOnPoint compile: 'extension ^ 73'.

	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test01ExtensionConflictsWithPreExistingNormalMethod04NormalMethodWorksAsExpectedAfterRecompilation [

	pointClass compile: 'extension ^ 17'.
	extensionOnPoint compile: 'extension ^ 73'.
	
	"Recompilation should not change anything"
	pointClass compile: 'extension ^ 17'.

	self assert: pointClass new extension equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test01ExtensionConflictsWithPreExistingNormalMethod05ExtensionThrowsExceptionAfterRecompilation [

	| scopedSelector |
	pointClass compile: 'extension ^ 17'.
	scopedSelector := extensionOnPoint compile: 'extension ^ 73'.
	
	"Recompilation should not change anything"
	pointClass compile: 'extension ^ 17'.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: { extensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test01ExtensionConflictsWithPreExistingNormalMethod06SubclassesAreStillAliasedAfterRecompilation [

	| scopedSelector |
	pointClass compile: 'extension ^ 17'.
	coloredPointClass compile: 'extension ^ 42'.
	scopedSelector := extensionOnPoint compile: 'extension ^ 73'.
	
	"Recompilation should not change anything"
	pointClass compile: 'extension ^ 17'.

	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test02NormalMethodConflictsWithPreExistingExtension01NormalMethodWorksAsExpected [

	extensionOnPoint compile: 'extension ^ 73'.
	pointClass compile: 'extension ^ 17'.

	self assert: pointClass new extension equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test02NormalMethodConflictsWithPreExistingExtension02ExtensionThrowsException [

	| scopedSelector |
	scopedSelector := extensionOnPoint compile: 'extension ^ 73'.
	pointClass compile: 'extension ^ 17'.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: { extensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test02NormalMethodConflictsWithPreExistingExtension03SubclassesAreStillAliased [
	
	| scopedSelector |
	scopedSelector := extensionOnPoint compile: 'extension ^ 73'.
	pointClass compile: 'extension ^ 17'.
	coloredPointClass compile: 'extension ^ 42'.

	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test02NormalMethodConflictsWithPreExistingExtension04NormalMethodWorksAsExpectedAfterRecompilation [

	extensionOnPoint compile: 'extension ^ 73'.
	pointClass compile: 'extension ^ 17'.
	
	"Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	self assert: pointClass new extension equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test02NormalMethodConflictsWithPreExistingExtension05ExtensionThrowsExceptionAfterRecompilation [

	| scopedSelector |
	scopedSelector := extensionOnPoint compile: 'extension ^ 73'.
	pointClass compile: 'extension ^ 17'.
	scopedSelector := extensionOnPoint compile: 'extension ^ 73'.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: { extensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test02NormalMethodConflictsWithPreExistingExtension06SubclassesAreStillAliasedAfterRecompilation [
	
	| scopedSelector |
	scopedSelector := extensionOnPoint compile: 'extension ^ 73'.
	pointClass compile: 'extension ^ 17'.
	coloredPointClass compile: 'extension ^ 42'.

	"Recompilation should not change anything"
	scopedSelector := extensionOnPoint compile: 'extension ^ 73'.

	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test03ImportExtensionMethodConflictsWithPreExistingImportedExtension01ExtensionThrowsException [
	"We have a first extension"

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	extensionOnPoint compile: 'extension ^ 73'. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnPoint.
					anotherExtensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test03ImportExtensionMethodConflictsWithPreExistingImportedExtension02KeepFirstImportWorkingOnIsolation [
	"We have a first extension"

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	extensionOnPoint compile: 'extension ^ 73'. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test03ImportExtensionMethodConflictsWithPreExistingImportedExtension03KeepSecondImportWorkingOnIsolation [
	"We have a first extension"

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	extensionOnPoint compile: 'extension ^ 73'. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint.

	scopedSelector := anotherExtensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test03ImportExtensionMethodConflictsWithPreExistingImportedExtension04SubclassesAreStillAliased [
	"We have a first extension"

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	extensionOnPoint compile: 'extension ^ 73'.
	coloredPointClass compile: 'extension ^ 42'. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test03ImportExtensionMethodConflictsWithPreExistingImportedExtension05ExtensionThrowsExceptionAfterRecompilation [
	"We have a first extension"

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	extensionOnPoint compile: 'extension ^ 73'. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.
	scopedSelector := userPackage visibleSelectorForSymbol: #extension.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnPoint.
					anotherExtensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test03ImportExtensionMethodConflictsWithPreExistingImportedExtension06KeepFirstImportWorkingOnIsolationAfterRecompilation [
	"We have a first extension"

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	extensionOnPoint compile: 'extension ^ 73'. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test03ImportExtensionMethodConflictsWithPreExistingImportedExtension07KeepSecondImportWorkingOnIsolationAfterRecompilation [
	"We have a first extension"

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	extensionOnPoint compile: 'extension ^ 73'. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := anotherExtensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test03ImportExtensionMethodConflictsWithPreExistingImportedExtension08SubclassesAreStillAliasedAfterRecompilation [
	"We have a first extension"

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	extensionOnPoint compile: 'extension ^ 73'.
	coloredPointClass compile: 'extension ^ 42'. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension01ExtensionThrowsException [

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	"We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	extensionOnPoint compile: 'extension ^ 73'.
	anotherExtensionOnPoint compile: 'extension ^ 17'.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnPoint.
					anotherExtensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension02KeepFirstImportWorkingOnIsolation [

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	"We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	extensionOnPoint compile: 'extension ^ 73'.
	anotherExtensionOnPoint compile: 'extension ^ 17'.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension03KeepSecondImportWorkingOnIsolation [

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	"We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	extensionOnPoint compile: 'extension ^ 73'.
	anotherExtensionOnPoint compile: 'extension ^ 17'.

	scopedSelector := anotherExtensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension04SubclassesAreStillAliased [

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	coloredPointClass compile: 'extension ^ 42'. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	extensionOnPoint compile: 'extension ^ 73'.
	anotherExtensionOnPoint compile: 'extension ^ 17'.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension05ExtensionThrowsExceptionAfterRecompilation [

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	"We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	extensionOnPoint compile: 'extension ^ 73'.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnPoint.
					anotherExtensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension06KeepFirstImportWorkingOnIsolationAfterRecompilation [

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	"We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	extensionOnPoint compile: 'extension ^ 73'.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension07KeepSecondImportWorkingOnIsolationAfterRecompilation [

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	"We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	extensionOnPoint compile: 'extension ^ 73'.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := anotherExtensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension08SubclassesAreStillAliasedAfterRecompilation [

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	coloredPointClass compile: 'extension ^ 42'. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage2) install. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	extensionOnPoint compile: 'extension ^ 73'.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test05LocalExtensionMethodConflictsWithPreExistingImportedExtension01ExtensionThrowsException [
	"We have a first extension"

	| anotherExtensionOnPoint scopedSelector |
	extensionOnPoint compile: 'extension ^ 73'.
	userPackage addExtensionImport: extensionOnPoint. "We have a second extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnPoint.
					anotherExtensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test05LocalExtensionMethodConflictsWithPreExistingImportedExtension02KeepImportWorkingOnIsolation [
	"We have a first extension"

	| anotherExtensionOnPoint scopedSelector |
	extensionOnPoint compile: 'extension ^ 73'.
	userPackage addExtensionImport: extensionOnPoint. "We have a second extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test05LocalExtensionMethodConflictsWithPreExistingImportedExtension03SubclassesAreStillAliased [

	| anotherExtensionOnPoint scopedSelector |
	coloredPointClass compile: 'extension ^ 42'.
	
	"We have a first extension"
	extensionOnPoint compile: 'extension ^ 73'.
	userPackage addExtensionImport: extensionOnPoint.
	
	"We have a second extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test05LocalExtensionMethodConflictsWithPreExistingImportedExtension04ExtensionThrowsExceptionAfterRecompilation [
	"We have a first extension"

	| anotherExtensionOnPoint scopedSelector |
	extensionOnPoint compile: 'extension ^ 73'.
	userPackage addExtensionImport: extensionOnPoint. "We have a second extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnPoint.
					anotherExtensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test05LocalExtensionMethodConflictsWithPreExistingImportedExtension05KeepImportWorkingOnIsolationAfterRecompilation [

	"We have a first extension"
	| anotherExtensionOnPoint scopedSelector |
	extensionOnPoint compile: 'extension ^ 73'.
	userPackage addExtensionImport: extensionOnPoint.

	"We have a second extension"
	anotherExtensionOnPoint := (Extension << #ExtensionOfPoint2
		extendedClass: pointClass;
		installingEnvironment: environment;
		package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'.
	
	"Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.
	
	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test05LocalExtensionMethodConflictsWithPreExistingImportedExtension06SubclassesAreStillAliasedAfterRecompilation [

	| anotherExtensionOnPoint scopedSelector |
	coloredPointClass compile: 'extension ^ 42'. "We have a first extension"
	extensionOnPoint compile: 'extension ^ 73'.
	userPackage addExtensionImport: extensionOnPoint. "We have a second extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test06ImportedExtensionConflictsWithPreExistingLocalExtensionMethod01ExtensionThrowsException [

	| anotherExtensionOnPoint scopedSelector |
	"We have a local extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "And we import a conflict"
	extensionOnPoint compile: 'extension ^ 73'.
	userPackage addExtensionImport: extensionOnPoint.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnPoint.
					anotherExtensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test06ImportedExtensionConflictsWithPreExistingLocalExtensionMethod02KeepImportWorkingOnIsolation [

	| anotherExtensionOnPoint scopedSelector |
	"We have a local extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "And we import a conflict"
	extensionOnPoint compile: 'extension ^ 73'.
	userPackage addExtensionImport: extensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test06ImportedExtensionConflictsWithPreExistingLocalExtensionMethod03SubclassesAreStillAliased [

	| anotherExtensionOnPoint scopedSelector |
	coloredPointClass compile: 'extension ^ 42'. "We have a local extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "And we import a conflict"
	extensionOnPoint compile: 'extension ^ 73'.
	userPackage addExtensionImport: extensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test06ImportedExtensionConflictsWithPreExistingLocalExtensionMethod04ExtensionThrowsExceptionAfterRecompilation [

	| anotherExtensionOnPoint scopedSelector |
	"We have a local extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "And we import a conflict"
	extensionOnPoint compile: 'extension ^ 73'.
	userPackage addExtensionImport: extensionOnPoint. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnPoint.
					anotherExtensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test06ImportedExtensionConflictsWithPreExistingLocalExtensionMethod05KeepImportWorkingOnIsolationAfterRecompilation [

	| anotherExtensionOnPoint scopedSelector |
	"We have a local extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "And we import a conflict"
	extensionOnPoint compile: 'extension ^ 73'.
	userPackage addExtensionImport: extensionOnPoint. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test06ImportedExtensionConflictsWithPreExistingLocalExtensionMethod06SubclassesAreStillAliasedAfterRecompilation [

	| anotherExtensionOnPoint scopedSelector |
	coloredPointClass compile: 'extension ^ 42'. "We have a local extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "And we import a conflict"
	extensionOnPoint compile: 'extension ^ 73'.
	userPackage addExtensionImport: extensionOnPoint. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test07NewExtensionOnImportExtensionMethodConflictsWithPreExistingLocalExtensionMethod01ExtensionThrowsException [

	| anotherExtensionOnPoint scopedSelector |
	"We have a local extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "We import first, then define a conflict"
	userPackage addExtensionImport: extensionOnPoint.
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnPoint.
					anotherExtensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test07NewExtensionOnImportExtensionMethodConflictsWithPreExistingLocalExtensionMethod02KeepImportWorkingOnIsolation [

	| anotherExtensionOnPoint scopedSelector |
	"We have a local extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "We import first, then define a conflict"
	userPackage addExtensionImport: extensionOnPoint.
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test07NewExtensionOnImportExtensionMethodConflictsWithPreExistingLocalExtensionMethod03SubclassesAreStillAliased [

	| anotherExtensionOnPoint scopedSelector |
	coloredPointClass compile: 'extension ^ 42'. "We have a local extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "We import first, then define a conflict"
	userPackage addExtensionImport: extensionOnPoint.
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test07NewExtensionOnImportExtensionMethodConflictsWithPreExistingLocalExtensionMethod04ExtensionThrowsExceptionAfterRecompilation [

	| anotherExtensionOnPoint scopedSelector |
	"We have a local extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "We import first, then define a conflict"
	userPackage addExtensionImport: extensionOnPoint.
	extensionOnPoint compile: 'extension ^ 73'. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnPoint.
					anotherExtensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test07NewExtensionOnImportExtensionMethodConflictsWithPreExistingLocalExtensionMethod05KeepImportWorkingOnIsolationAfterRecompilation [

	| anotherExtensionOnPoint scopedSelector |
	"We have a local extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "We import first, then define a conflict"
	userPackage addExtensionImport: extensionOnPoint.
	extensionOnPoint compile: 'extension ^ 73'. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test07NewExtensionOnImportExtensionMethodConflictsWithPreExistingLocalExtensionMethod06SubclassesAreStillAliasedAfterRecompilation [

	| anotherExtensionOnPoint scopedSelector |
	coloredPointClass compile: 'extension ^ 42'. "We have a local extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: userPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "We import first, then define a conflict"
	userPackage addExtensionImport: extensionOnPoint.
	extensionOnPoint compile: 'extension ^ 73'. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test08LocalExtensionMethodConflictsWithPreExistingLocalExtension01ExtensionThrowsException [
	"We have a first extension"

	| anotherExtensionOnPoint scopedSelector |
	extensionOnPoint compile: 'extension ^ 73'. "We have a second extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'.

	scopedSelector := definerPackage visibleSelectorForSymbol: #extension.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnPoint.
					anotherExtensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test08LocalExtensionMethodConflictsWithPreExistingLocalExtension02SubclassesAreStillAliased [

	| anotherExtensionOnPoint scopedSelector |
	coloredPointClass compile: 'extension ^ 42'. "We have a first extension"
	extensionOnPoint compile: 'extension ^ 73'. "We have a second extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'.

	scopedSelector := definerPackage visibleSelectorForSymbol: #extension.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test08LocalExtensionMethodConflictsWithPreExistingLocalExtension03ExtensionThrowsExceptionAfterRecompilation [
	"We have a first extension"

	| anotherExtensionOnPoint scopedSelector |
	extensionOnPoint compile: 'extension ^ 73'. "We have a second extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := definerPackage visibleSelectorForSymbol: #extension.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnPoint.
					anotherExtensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test08LocalExtensionMethodConflictsWithPreExistingLocalExtension04SubclassesAreStillAliasedAfterRecompilation [

	| anotherExtensionOnPoint scopedSelector |
	coloredPointClass compile: 'extension ^ 42'. "We have a first extension"
	extensionOnPoint compile: 'extension ^ 73'. "We have a second extension"
	anotherExtensionOnPoint := ((Extension << #ExtensionOfPoint2)
		                            extendedClass: pointClass;
		                            installingEnvironment: environment;
		                            package: definerPackage) install.
	anotherExtensionOnPoint compile: 'extension ^ 17'. "Recompilation should not change anything"
	extensionOnPoint compile: 'extension ^ 73'.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]
