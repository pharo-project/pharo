Class {
	#name : 'ExtensionHotUpdateAliasingTest',
	#superclass : 'TestCase',
	#instVars : [
		'environment',
		'definerPackage',
		'userPackage',
		'objectClass',
		'pointClass',
		'extensionOnPoint',
		'extensionOnObject',
		'userClass',
		'coloredPointClass',
		'extensionOnColoredPoint',
		'stringClass',
		'nicerStringClass'
	],
	#category : 'Kernel-CodeModel-Tests-Extensions',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Extensions'
}

{ #category : 'running' }
ExtensionHotUpdateAliasingTest >> setUp [

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
ExtensionHotUpdateAliasingTest >> tearDown [
	"Make sure we did not pollute the global dictionaries"

	self deny: (Point methodDictionary keys anySatisfy: [ :k | k isKindOf: ScopedSelector ]).
	self deny: (Object methodDictionary keys anySatisfy: [ :k | k isKindOf: ScopedSelector ]).
	self deny: (Integer methodDictionary keys anySatisfy: [ :k | k isKindOf: ScopedSelector ]).
	
	environment allClassesAndTraits do: [ :e | e removeFromSystem ].
	
	super tearDown
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test01CompileNewExtensionMethod01InstallsExtensionInExtendedClass [

	| scopedSelector |
	scopedSelector := extensionOnObject compile: 'extension ^ 42'.
	
	self assert: (objectClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test01CompileNewExtensionMethod02InstallsAliasInSubclasses [

	"Point defines a normal method"
	| scopedSelector |
	pointClass compile: 'extension ^ 17'.
	
	"We extend the superclass with the same selector"
	scopedSelector := extensionOnObject compile: 'extension ^ 42'.
	
	
	self assert: (pointClass new perform: scopedSelector) equals: 17
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test01CompileNewExtensionMethod03InstallsAliasInSubclassesButStopsAtFrontier [

	| scopedSelector |
	
	"Point defines a normal method that should be aliased"
	pointClass compile: 'extension ^ 17'.
	extensionOnColoredPoint compile: 'extension ^ 73'.
	
	"We extend the superclass with the same selector"
	scopedSelector := extensionOnObject compile: 'extension ^ 42'.
	
	self assert: (pointClass new perform: scopedSelector) equals: 17.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 73.
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test02OverrideAliasWithNormalMethod01ShouldAliasNewMethod [

	| scopedSelector |
	"We extend Point"
	scopedSelector := extensionOnPoint compile: 'extension ^ 11'.
	"String defines a normal method with the same selector name, global"
	stringClass compile: 'extension ^ 42'.
	
	"We send a message to a string to alias the method"
	self assert: (stringClass new perform: scopedSelector) equals: 42.
	
	"Now we override #extension in MyNicerString and that should automatically alias it!"
	nicerStringClass compile: 'extension ^ 73'.
	
	self assert: (nicerStringClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test02OverrideExtensionWithNormalMethod01ShouldAliasNewMethod [

	| scopedSelector |
	"We extend Object"
	scopedSelector := extensionOnObject compile: 'extension ^ 11'.
	
	"Now we override the extension.
	This should alias the one in the superclass: the scoped selector should see this method"
	pointClass compile: 'extension ^ 42'.
	
	self assert: (pointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test03OnDemandAlias01ShouldAliasSubclasses [

	| scopedSelector |
	"We extend Point"
	scopedSelector := extensionOnPoint compile: 'extension ^ 11'.
	"We define two methods in the hierarchy"
	stringClass compile: 'extension ^ 42'.	
	nicerStringClass compile: 'extension ^ 73'.
	
	"We send a message to a string to alias the method."
	"In addition, it should automatically alias the subclass!"
	self assert: (stringClass new perform: scopedSelector) equals: 42.	
	self assert: (nicerStringClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test03OnDemandAlias02ShouldAliasConflicts [

	| scopedSelector userScopedSelector |
	pointClass compile: 'extension ^ 73'.
	coloredPointClass compile: 'extension ^ 11'.
	scopedSelector := extensionOnColoredPoint compile: 'extension ^ 42'.

	self assert: pointClass new extension equals: 73.
	self assert: coloredPointClass new extension equals: 11.

	self assert: (pointClass new perform: scopedSelector) equals: 73.
	self should: [ coloredPointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyColoredPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnColoredPoint } ].

	userPackage addExtensionImport: extensionOnColoredPoint.
	userScopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self should: [ coloredPointClass new perform: userScopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyColoredPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: {
					extensionOnColoredPoint } ].
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test03RemoveExtension01ShouldRemoveExtensionMethod [

	| scopedSelector receiver |
	"We extend Object"
	scopedSelector := extensionOnObject compile: 'extension ^ 11'.
	
	"Now we remove the extension"
	extensionOnObject removeSelector: scopedSelector.
	
	"So the message is not understood anymore"
	receiver := objectClass new.
	self
		should: [ receiver perform: scopedSelector ]
		raise: MessageNotUnderstood
		withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: scopedSelector ]
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test03RemoveExtension02ShouldKeepAliasInSubclass [

	| scopedSelector |
	"We extend Object"
	scopedSelector := extensionOnObject internScopedSelector: #extension.
	extensionOnObject compile: 'extension ^ 11'.
	
	"Now we override the extension.
	This should alias the one in the superclass: the scoped selector should see this method"
	pointClass compile: 'extension ^ 42'.
	
	extensionOnObject removeSelector: scopedSelector.
	
	self assert: (pointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test03RemoveExtension03ShouldNotRemoveUnrelatedAliases [

	| extensionScopedSelector receiver userScopedSelector definerPackage2 extensionOnObject2 userPackage2 extensionScopedSelector2 userScopedSelector2 |
	"Complex setup.
	We have two packages defining extensions (def1, def2).
	Two users (user1, user2).
	Each user uses only one extension, and thus has an alias to it.
	
	We remove one of the extensions: def1.
	This should not impact def2.
	""First package already exists.
	Add extension method, import, send to alias"
	extensionOnObject compile: 'extension ^ 25'.
	userPackage addExtensionImport: extensionOnObject.
	extensionScopedSelector := definerPackage visibleSelectorForSymbol: #extension.
	userScopedSelector := userPackage visibleSelectorForSymbol: #extension.
	objectClass new perform: userScopedSelector. "Second package.
	Create package and extension"
	definerPackage2 := Package named: 'Definer2'.
	extensionOnObject2 := ((Extension << #ExtensionOfObject2)
		                       extendedClass: objectClass;
		                       installingEnvironment: environment;
		                       package: definerPackage2) install. "Add extension method, import, send to alias"
	extensionOnObject2 compile: 'extension ^ 17'.
	userPackage2 := Package named: 'User2'.
	userPackage2 addExtensionImport: extensionOnObject2.
	extensionScopedSelector2 := definerPackage2 visibleSelectorForSymbol: #extension.
	userScopedSelector2 := userPackage2 visibleSelectorForSymbol: #extension.
	objectClass new perform: userScopedSelector2. "Now we remove the extension from the definer package 1"
	extensionOnObject removeSelector: extensionScopedSelector.

	receiver := objectClass new. "So the message from definer package 1 is not understood anymore"
	self should: [ receiver perform: extensionScopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: extensionScopedSelector ]. "Nor from the user 1 alias"
	self should: [ receiver perform: userScopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: userScopedSelector ]. "But it should still understand the messages from the definer package 2 and its user"
	self assert: (receiver perform: extensionScopedSelector2) equals: 17.
	self assert: (receiver perform: userScopedSelector2) equals: 17
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test03RemoveExtension04ShouldRemoveAliasesToExtensionInTheExtendedClass [

	| extensionScopedSelector receiver userScopedSelector |
	"We extend Object"
	extensionScopedSelector := extensionOnObject compile: 'extension ^ 11'.
	"We import and use the extension, which will generate an alias"
	userPackage addExtensionImport: extensionOnObject.
	userClass compile: 'userMethod: anObject ^ anObject extension'.
	
	userScopedSelector := userPackage visibleSelectorForSymbol: #extension.
	
	"Send the message to create the alias"
	userClass new perform: #userMethod: with: objectClass new.
	
	"Now we remove the extension"
	extensionOnObject removeSelector: extensionScopedSelector.
	"So the message is not understood anymore"
	receiver := objectClass new.
	
	"Not from the package itself"
	self should: [ receiver perform: extensionScopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: extensionScopedSelector ].

	"Nor from the user alias"
	self should: [ receiver perform: userScopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: userScopedSelector ]
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test04ImportExtensionMethod01InstallsExtensionInExtendedClass [

	| userScopedSelector |
	extensionOnObject compile: 'extension ^ 42'.
	userPackage addExtensionImport: extensionOnObject.

	userScopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: userScopedSelector owner == userPackage.

	self assert: (objectClass new perform: userScopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test05CompileNewExtensionMethodAfterImport01InstallsExtensionInExtendedClass [

	| userScopedSelector |
	userPackage addExtensionImport: extensionOnObject.
	extensionOnObject compile: 'extension ^ 42'.

	userScopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: userScopedSelector owner == userPackage.

	self assert: (objectClass new perform: userScopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test06RemoveAliasedMethod02RemovesAliases [

	"Point defines a normal method"
	| scopedSelector |
	pointClass compile: 'extension ^ 17'.
	
	"We extend the superclass with the same selector"
	scopedSelector := extensionOnObject compile: 'extension ^ 42'.
	
	pointClass removeSelector: #extension.
	
	self assert: (pointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test07RemoveMethodAliasedOnDemand01RemovesAliases [

	| scopedSelector receiver |
	"We extend Point"
	scopedSelector := extensionOnPoint compile: 'extension ^ 11'. "We define two methods in the hierarchy"
	stringClass compile: 'extension ^ 42'.
	nicerStringClass compile: 'extension ^ 73'. "We send a message to a string to alias the method." "In addition, it should automatically alias the subclass!"
	self assert: (stringClass new perform: scopedSelector) equals: 42.
	self assert: (nicerStringClass new perform: scopedSelector) equals: 73.

	stringClass removeSelector: #extension.

	receiver := stringClass new.
	self should: [ receiver perform: scopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :ex |
			self assert: ex receiver == receiver.
			self assert: ex message selector == scopedSelector ]
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test07RemoveMethodAliasedOnDemand01RemovesAliasesInSubclasses [

	| scopedSelector |
	"We extend Point"
	scopedSelector := extensionOnPoint compile: 'extension ^ 11'. "We define two methods in the hierarchy"
	stringClass compile: 'extension ^ 42'.
	nicerStringClass compile: 'extension ^ 73'. "We send a message to a string to alias the method." "In addition, it should automatically alias the subclass!"
	self assert: (stringClass new perform: scopedSelector) equals: 42.
	self assert: (nicerStringClass new perform: scopedSelector) equals: 73.

	nicerStringClass removeSelector: #extension.

	self assert: (nicerStringClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test08RemoveExtensionMethodFromImport01ShouldRemoveExtensionMethodFromImporterScope [

	| scopedSelector receiver |
	"We extend Object"
	extensionOnObject compile: 'extension ^ 11'. "We import it -> the method should be locally seen using the local scoped selector"
	userPackage addExtensionImport: extensionOnObject.
	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (objectClass new perform: scopedSelector) equals: 11. "We remove the extension. Now the object should not understand the message anymore"
	extensionOnObject removeSelector: (definerPackage visibleSelectorForSymbol: #extension). "So the message is not understood anymore"
	receiver := objectClass new.
	self should: [ receiver perform: scopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: scopedSelector ]
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test08RemoveExtensionMethodFromImport02ShouldNotRemoveOriginalExtensionMethod [

	| scopedSelector receiver |
	"We extend Object"
	scopedSelector := extensionOnObject compile: 'extension ^ 11'.

	userPackage addExtensionImport: extensionOnObject.
	self assert: (objectClass new perform: scopedSelector) equals: 11. "We remove the extension. Now the object should not understand the message anymore"
	extensionOnObject removeSelector: (definerPackage visibleSelectorForSymbol: #extension). "So the message is not understood anymore"
	receiver := objectClass new.
	self should: [ receiver perform: scopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: scopedSelector ]
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test09RemoveImport01ShouldRemoveExtensionMethod [

	| scopedSelector receiver |
	"We extend Object"
	extensionOnObject compile: 'extension ^ 11'. "We import it -> the method should be locally seen using the local scoped selector"
	userPackage addExtensionImport: extensionOnObject.
	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (objectClass new perform: scopedSelector) equals: 11. "We remove the extension. Now the object should not understand the message anymore"
	userPackage removeExtensionImport: extensionOnObject. "So the message is not understood anymore"
	receiver := objectClass new.
	self should: [ receiver perform: scopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: scopedSelector ]
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test09RemoveImport02ShouldNotRemoveOriginalExtensionMethod [

	| scopedSelector |
	"We extend Object"
	scopedSelector := extensionOnObject compile: 'extension ^ 11'.

	userPackage addExtensionImport: extensionOnObject.
	self assert: (objectClass new perform: scopedSelector) equals: 11.
	
	"We remove the extension. Now the object should not understand the message anymore"
	userPackage removeExtensionImport: extensionOnObject.
		
	"So message from the extension package should still work"
	self assert: (objectClass new perform: scopedSelector) equals: 11.
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test10RemoveExtensionFromSystem01ShouldRemoveExtensionMethodFromImporterScope [

	| scopedSelector receiver |
	"We extend Object"
	extensionOnObject compile: 'extension ^ 11'. "We import it -> the method should be locally seen using the local scoped selector"
	userPackage addExtensionImport: extensionOnObject.
	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (objectClass new perform: scopedSelector) equals: 11. "We remove the extension. Now the object should not understand the message anymore"
	extensionOnObject removeFromSystem. "So the message is not understood anymore"
	receiver := objectClass new.
	self should: [ receiver perform: scopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: scopedSelector ]
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test10RemoveExtensionFromSystem02ShouldNotRemoveOriginalExtensionMethod [

	| scopedSelector receiver |
	"We extend Object"
	scopedSelector := extensionOnObject compile: 'extension ^ 11'.

	userPackage addExtensionImport: extensionOnObject.
	self assert: (objectClass new perform: scopedSelector) equals: 11.
	
	"We remove the extension. Now the object should not understand the message anymore"
	extensionOnObject removeFromSystem.
		
	"So the message is not understood anymore"
	receiver := objectClass new.
	self
		should: [ receiver perform: scopedSelector ]
		raise: MessageNotUnderstood
		withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: scopedSelector ]
]
