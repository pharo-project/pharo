Class {
	#name : 'ExtensionConflictTest',
	#superclass : 'AbstractExtensionTest',
	#instVars : [
		'pointClass',
		'extensionOnPoint',
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

	super setUp.

	objectClass := self createClassNamed: #MyObject superclass: isolatingRoot package: userPackage.
	pointClass := self createClassNamed: #MyPoint superclass: objectClass package: userPackage.
	coloredPointClass := self createClassNamed: #MyColoredPoint superclass: pointClass package: userPackage.
	stringClass := self createClassNamed: #MyString superclass: objectClass package: userPackage.
	
	"String will have no extensions. It's here just for polymorphism.
	It will thus contain aliases only"
	nicerStringClass := self createClassNamed: #MyNicerString superclass: stringClass package: userPackage.
	userClass := self createClassNamed: #MyUserClass superclass: objectClass package: userPackage.


	extensionOnObject := self createExtensionNamed: #ExtensionOfObject forBehavior: objectClass inPackage: definerPackage.
	extensionOnPoint := self createExtensionNamed: #ExtensionOfPoint forBehavior: pointClass inPackage: definerPackage.
	extensionOnColoredPoint := self createExtensionNamed: #ExtensionOfColoredPoint forBehavior: coloredPointClass inPackage: definerPackage
]

{ #category : 'tests' }
ExtensionConflictTest >> test01ExtensionConflictsWithPreExistingNormalMethod01NormalMethodWorksAsExpected [

	self compile: 'extension ^ 17' on: pointClass.
	self compile: 'extension ^ 73' on: extensionOnPoint.

	self assert: pointClass new extension equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test01ExtensionConflictsWithPreExistingNormalMethod02ExtensionThrowsException [

	| scopedSelector |
	self compile: 'extension ^ 17' on: pointClass.
	scopedSelector := self compile: 'extension ^ 73' on: extensionOnPoint.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: { extensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test01ExtensionConflictsWithPreExistingNormalMethod03SubclassesAreStillAliased [

	| scopedSelector |
	self compile: 'extension ^ 17' on: pointClass.
	self compile: 'extension ^ 42' on: coloredPointClass.
	scopedSelector := self compile: 'extension ^ 73' on: extensionOnPoint.

	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test01ExtensionConflictsWithPreExistingNormalMethod04NormalMethodWorksAsExpectedAfterRecompilation [

	self compile: 'extension ^ 17' on: pointClass.
	self compile: 'extension ^ 73' on: extensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 17' on: pointClass.

	self assert: pointClass new extension equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test01ExtensionConflictsWithPreExistingNormalMethod05ExtensionThrowsExceptionAfterRecompilation [

	| scopedSelector |
	self compile: 'extension ^ 17' on: pointClass.
	scopedSelector := self compile: 'extension ^ 73' on: extensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 17' on: pointClass.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: { extensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test01ExtensionConflictsWithPreExistingNormalMethod06SubclassesAreStillAliasedAfterRecompilation [

	| scopedSelector |
	self compile: 'extension ^ 17' on: pointClass.
	self compile: 'extension ^ 42' on: coloredPointClass.
	scopedSelector := self compile: 'extension ^ 73' on: extensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 17' on: pointClass.

	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test02NormalMethodConflictsWithPreExistingExtension01NormalMethodWorksAsExpected [

	self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 17' on: pointClass.

	self assert: pointClass new extension equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test02NormalMethodConflictsWithPreExistingExtension02ExtensionThrowsException [

	| scopedSelector |
	scopedSelector := self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 17' on: pointClass.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: { extensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test02NormalMethodConflictsWithPreExistingExtension03SubclassesAreStillAliased [

	| scopedSelector |
	scopedSelector := self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 17' on: pointClass.
	self compile: 'extension ^ 42' on: coloredPointClass.

	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test02NormalMethodConflictsWithPreExistingExtension04NormalMethodWorksAsExpectedAfterRecompilation [

	self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 17' on: pointClass. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

	self assert: pointClass new extension equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test02NormalMethodConflictsWithPreExistingExtension05ExtensionThrowsExceptionAfterRecompilation [

	| scopedSelector |
	scopedSelector := self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 17' on: pointClass.
	scopedSelector := self compile: 'extension ^ 73' on: extensionOnPoint.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: { extensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test02NormalMethodConflictsWithPreExistingExtension06SubclassesAreStillAliasedAfterRecompilation [

	| scopedSelector |
	scopedSelector := self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 17' on: pointClass.
	self compile: 'extension ^ 42' on: coloredPointClass. "Recompilation should not change anything"
	scopedSelector := self compile: 'extension ^ 73' on: extensionOnPoint.

	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test03ImportExtensionMethodConflictsWithPreExistingImportedExtension01ExtensionThrowsException [
	"We have a first extension"

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 73' on: extensionOnPoint. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "We import both extensions"
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
	self compile: 'extension ^ 73' on: extensionOnPoint. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test03ImportExtensionMethodConflictsWithPreExistingImportedExtension03KeepSecondImportWorkingOnIsolation [
	"We have a first extension"

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 73' on: extensionOnPoint. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint.

	scopedSelector := anotherExtensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test03ImportExtensionMethodConflictsWithPreExistingImportedExtension04SubclassesAreStillAliased [
	"We have a first extension"

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 42' on: coloredPointClass. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test03ImportExtensionMethodConflictsWithPreExistingImportedExtension05ExtensionThrowsExceptionAfterRecompilation [
	"We have a first extension"

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 73' on: extensionOnPoint. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.
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
	self compile: 'extension ^ 73' on: extensionOnPoint. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test03ImportExtensionMethodConflictsWithPreExistingImportedExtension07KeepSecondImportWorkingOnIsolationAfterRecompilation [
	"We have a first extension"

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 73' on: extensionOnPoint. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := anotherExtensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test03ImportExtensionMethodConflictsWithPreExistingImportedExtension08SubclassesAreStillAliasedAfterRecompilation [
	"We have a first extension"

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 42' on: coloredPointClass. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint.

	"We import both extensions"
	userPackage addExtensionImport: extensionOnPoint.
	userPackage addExtensionImport: anotherExtensionOnPoint.
	
	"Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension01ExtensionThrowsException [

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	"We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	userPackage addExtensionImport: extensionOnPoint. "We import both extensions"
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.

	self should: [ pointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnPoint.
					anotherExtensionOnPoint } ]
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension02KeepFirstImportWorkingOnIsolation [

	| scopedSelector anotherExtensionOnPoint |
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.

	userPackage addExtensionImport: extensionOnPoint. "We import both extensions"
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension03KeepSecondImportWorkingOnIsolation [

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	"We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	userPackage addExtensionImport: extensionOnPoint. "We import both extensions"
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint.

	scopedSelector := anotherExtensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension04SubclassesAreStillAliased [

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 42' on: coloredPointClass. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	userPackage addExtensionImport: extensionOnPoint. "We import both extensions"
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension05ExtensionThrowsExceptionAfterRecompilation [

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	"We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	userPackage addExtensionImport: extensionOnPoint. "We import both extensions"
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

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
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	userPackage addExtensionImport: extensionOnPoint. "We import both extensions"
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension07KeepSecondImportWorkingOnIsolationAfterRecompilation [

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	"We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	userPackage addExtensionImport: extensionOnPoint. "We import both extensions"
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := anotherExtensionOnPoint package visibleSelectorForSymbol: #extension.
	self assert: (pointClass new perform: scopedSelector) equals: 17
]

{ #category : 'tests' }
ExtensionConflictTest >> test04NewExtensionOnImportExtensionMethodConflictsWithPreExistingImportedExtension08SubclassesAreStillAliasedAfterRecompilation [

	| definerPackage2 anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 42' on: coloredPointClass. "We have a second extension"
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage2.
	userPackage addExtensionImport: extensionOnPoint. "We import both extensions"
	userPackage addExtensionImport: anotherExtensionOnPoint. "We define the methods on the extensions after import"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test05LocalExtensionMethodConflictsWithPreExistingImportedExtension01ExtensionThrowsException [
	"We have a first extension"

	| anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 73' on: extensionOnPoint.
	userPackage addExtensionImport: extensionOnPoint. "We have a second extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint.

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
	self compile: 'extension ^ 73' on: extensionOnPoint.
	userPackage addExtensionImport: extensionOnPoint. "We have a second extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test05LocalExtensionMethodConflictsWithPreExistingImportedExtension03SubclassesAreStillAliased [

	| anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 42' on: coloredPointClass. "We have a first extension"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	userPackage addExtensionImport: extensionOnPoint. "We have a second extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test05LocalExtensionMethodConflictsWithPreExistingImportedExtension04ExtensionThrowsExceptionAfterRecompilation [
	"We have a first extension"

	| anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 73' on: extensionOnPoint.
	userPackage addExtensionImport: extensionOnPoint. "We have a second extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

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
	self compile: 'extension ^ 73' on: extensionOnPoint.
	userPackage addExtensionImport: extensionOnPoint. "We have a second extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test05LocalExtensionMethodConflictsWithPreExistingImportedExtension06SubclassesAreStillAliasedAfterRecompilation [

	| anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 42' on: coloredPointClass. "We have a first extension"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	userPackage addExtensionImport: extensionOnPoint. "We have a second extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test06ImportedExtensionConflictsWithPreExistingLocalExtensionMethod01ExtensionThrowsException [

	| anotherExtensionOnPoint scopedSelector |
	"We have a local extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "And we import a conflict"
	self compile: 'extension ^ 73' on: extensionOnPoint.
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
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "And we import a conflict"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	userPackage addExtensionImport: extensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test06ImportedExtensionConflictsWithPreExistingLocalExtensionMethod03SubclassesAreStillAliased [

	| anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 42' on: coloredPointClass. "We have a local extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "And we import a conflict"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	userPackage addExtensionImport: extensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test06ImportedExtensionConflictsWithPreExistingLocalExtensionMethod04ExtensionThrowsExceptionAfterRecompilation [

	| anotherExtensionOnPoint scopedSelector |
	"We have a local extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "And we import a conflict"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	userPackage addExtensionImport: extensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

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
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "And we import a conflict"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	userPackage addExtensionImport: extensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test06ImportedExtensionConflictsWithPreExistingLocalExtensionMethod06SubclassesAreStillAliasedAfterRecompilation [

	| anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 42' on: coloredPointClass. "We have a local extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "And we import a conflict"
	self compile: 'extension ^ 73' on: extensionOnPoint.
	userPackage addExtensionImport: extensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test07NewExtensionOnImportExtensionMethodConflictsWithPreExistingLocalExtensionMethod01ExtensionThrowsException [

	| anotherExtensionOnPoint scopedSelector |
	"We have a local extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "We import first, then define a conflict"
	userPackage addExtensionImport: extensionOnPoint.
	self compile: 'extension ^ 73' on: extensionOnPoint.

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
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "We import first, then define a conflict"
	userPackage addExtensionImport: extensionOnPoint.
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test07NewExtensionOnImportExtensionMethodConflictsWithPreExistingLocalExtensionMethod03SubclassesAreStillAliased [

	| anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 42' on: coloredPointClass. "We have a local extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "We import first, then define a conflict"
	userPackage addExtensionImport: extensionOnPoint.
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test07NewExtensionOnImportExtensionMethodConflictsWithPreExistingLocalExtensionMethod04ExtensionThrowsExceptionAfterRecompilation [

	| anotherExtensionOnPoint scopedSelector |
	"We have a local extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "We import first, then define a conflict"
	userPackage addExtensionImport: extensionOnPoint.
	self compile: 'extension ^ 73' on: extensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

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
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "We import first, then define a conflict"
	userPackage addExtensionImport: extensionOnPoint.
	self compile: 'extension ^ 73' on: extensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	self assert: (pointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionConflictTest >> test07NewExtensionOnImportExtensionMethodConflictsWithPreExistingLocalExtensionMethod06SubclassesAreStillAliasedAfterRecompilation [

	| anotherExtensionOnPoint scopedSelector |
	self compile: 'extension ^ 42' on: coloredPointClass. "We have a local extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "We import first, then define a conflict"
	userPackage addExtensionImport: extensionOnPoint.
	self compile: 'extension ^ 73' on: extensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := extensionOnPoint package visibleSelectorForSymbol: #extension.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test08LocalExtensionMethodConflictsWithPreExistingLocalExtension01ExtensionThrowsException [
	"We have a first extension"

	| scopedSelector anotherExtensionOnPoint |
	self compile: 'extension ^ 73' on: extensionOnPoint.
	
	"We have a second extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint.

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
	self compile: 'extension ^ 42' on: coloredPointClass. "We have a first extension"
	self compile: 'extension ^ 73' on: extensionOnPoint. "We have a second extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint.

	scopedSelector := definerPackage visibleSelectorForSymbol: #extension.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionConflictTest >> test08LocalExtensionMethodConflictsWithPreExistingLocalExtension03ExtensionThrowsExceptionAfterRecompilation [
	"We have a first extension"

	| scopedSelector anotherExtensionOnPoint |
	self compile: 'extension ^ 73' on: extensionOnPoint.
	
	"We have a second extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: definerPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint.
	
	"Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

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
	self compile: 'extension ^ 42' on: coloredPointClass. "We have a first extension"
	self compile: 'extension ^ 73' on: extensionOnPoint. "We have a second extension"
	anotherExtensionOnPoint := self createExtensionNamed: #ExtensionOfPoint2 forBehavior: pointClass inPackage: userPackage.
	self compile: 'extension ^ 17' on: anotherExtensionOnPoint. "Recompilation should not change anything"
	self compile: 'extension ^ 73' on: extensionOnPoint.

	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 42
]
