Class {
	#name : 'ExtensionHotUpdateAliasingTest',
	#superclass : 'AbstractExtensionTest',
	#instVars : [
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
	extensionOnColoredPoint := self
		                           createExtensionNamed: #ExtensionOfColoredPoint
		                           forBehavior: coloredPointClass
		                           inPackage: definerPackage
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test01CompileNewExtensionMethod01InstallsExtensionInExtendedClass [

	| scopedSelector |
	scopedSelector := self compile: 'extension ^ 42' on: extensionOnObject.

	self assert: (objectClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test01CompileNewExtensionMethod02InstallsAliasInSubclasses [
	"Point defines a normal method"

	| scopedSelector |
	self compile: 'extension ^ 17' on: pointClass. "We extend the superclass with the same selector"
	scopedSelector := self compile: 'extension ^ 42' on: extensionOnObject.


	self assert: (pointClass new perform: scopedSelector) equals: 17
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test01CompileNewExtensionMethod03InstallsAliasInSubclassesButStopsAtFrontier [

	| scopedSelector |
	"Point defines a normal method that should be aliased"
	self compile: 'extension ^ 17' on: pointClass.
	self compile: 'extension ^ 73' on: extensionOnColoredPoint. "We extend the superclass with the same selector"
	scopedSelector := self compile: 'extension ^ 42' on: extensionOnObject.

	self assert: (pointClass new perform: scopedSelector) equals: 17.
	self assert: (coloredPointClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test02OverrideAliasWithNormalMethod01ShouldAliasNewMethod [

	| scopedSelector |
	"We extend Point"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnPoint. "String defines a normal method with the same selector name, global"
	self compile: 'extension ^ 42' on: stringClass. "We send a message to a string to alias the method"
	self assert: (stringClass new perform: scopedSelector) equals: 42. "Now we override #extension in MyNicerString and that should automatically alias it!"
	self compile: 'extension ^ 73' on: nicerStringClass.

	self assert: (nicerStringClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test02OverrideExtensionWithNormalMethod01ShouldAliasNewMethod [

	| scopedSelector |
	"We extend Object"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject. "Now we override the extension.
	This should alias the one in the superclass: the scoped selector should see this method"
	self compile: 'extension ^ 42' on: pointClass.

	self assert: (pointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test03OnDemandAlias01ShouldAliasSubclasses [

	| scopedSelector |
	"We extend Point"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnPoint. "We define two methods in the hierarchy"
	self compile: 'extension ^ 42' on: stringClass.
	self compile: 'extension ^ 73' on: nicerStringClass. "We send a message to a string to alias the method." "In addition, it should automatically alias the subclass!"
	self assert: (stringClass new perform: scopedSelector) equals: 42.
	self assert: (nicerStringClass new perform: scopedSelector) equals: 73
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test03OnDemandAlias02ShouldAliasConflicts [

	| scopedSelector userScopedSelector |
	self compile: 'extension ^ 73' on: pointClass.
	self compile: 'extension ^ 11' on: coloredPointClass.
	scopedSelector := self compile: 'extension ^ 42' on: extensionOnColoredPoint.

	self assert: pointClass new extension equals: 73.
	self assert: coloredPointClass new extension equals: 11.

	self assert: (pointClass new perform: scopedSelector) equals: 73.
	self should: [ coloredPointClass new perform: scopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyColoredPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: { extensionOnColoredPoint } ].

	userPackage addExtensionImport: extensionOnColoredPoint.
	userScopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self should: [ coloredPointClass new perform: userScopedSelector ] raise: ExtensionConflictError with: [ :conflict |
			self assert: conflict messageText equals: 'Conflict: extension overwrites method #extension in MyColoredPoint'.
			self assertCollection: conflict conflictingExtensions hasSameElements: { extensionOnColoredPoint } ]
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test03RemoveExtension01ShouldRemoveExtensionMethod [

	| scopedSelector receiver |
	"We extend Object"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject. "Now we remove the extension"
	extensionOnObject removeSelector: scopedSelector logged: false. "So the message is not understood anymore"
	receiver := objectClass new.
	self should: [ receiver perform: scopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: scopedSelector ]
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test03RemoveExtension02ShouldKeepAliasInSubclass [

	| scopedSelector |
	"We extend Object"
	scopedSelector := extensionOnObject internScopedSelector: #extension.
	self compile: 'extension ^ 11' on: extensionOnObject. "Now we override the extension.
	This should alias the one in the superclass: the scoped selector should see this method"
	self compile: 'extension ^ 42' on: pointClass.

	extensionOnObject removeSelector: scopedSelector logged: false.

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
	self compile: 'extension ^ 25' on: extensionOnObject.
	userPackage addExtensionImport: extensionOnObject.
	extensionScopedSelector := definerPackage visibleSelectorForSymbol: #extension.
	userScopedSelector := userPackage visibleSelectorForSymbol: #extension.
	objectClass new perform: userScopedSelector.
	
	"Second package.
	Create package and extension"
	definerPackage2 := Package named: 'Definer2'.
	extensionOnObject2 := self createExtensionNamed: #ExtensionOfObject2 forBehavior: objectClass inPackage: definerPackage2.
	
	"Add extension method, import, send to alias"
	self compile: 'extension ^ 17' on: extensionOnObject2.
	userPackage2 := Package named: 'User2'.
	userPackage2 addExtensionImport: extensionOnObject2.
	extensionScopedSelector2 := definerPackage2 visibleSelectorForSymbol: #extension.
	userScopedSelector2 := userPackage2 visibleSelectorForSymbol: #extension.
	objectClass new perform: userScopedSelector2. "Now we remove the extension from the definer package 1"
	extensionOnObject removeSelector: extensionScopedSelector logged: false.

	receiver := objectClass new.
	
	"So the message from definer package 1 is not understood anymore"
	self should: [ receiver perform: extensionScopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: extensionScopedSelector ].
		
	"Nor from the user 1 alias"
	self should: [ receiver perform: userScopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: userScopedSelector ].
		
	"But it should still understand the messages from the definer package 2 and its user"
	self assert: (receiver perform: extensionScopedSelector2) equals: 17.
	self assert: (receiver perform: userScopedSelector2) equals: 17
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test03RemoveExtension04ShouldRemoveAliasesToExtensionInTheExtendedClass [

	| extensionScopedSelector receiver userScopedSelector |
	"We extend Object"
	extensionScopedSelector := self compile: 'extension ^ 11' on: extensionOnObject.
	
	"We import and use the extension, which will generate an alias"
	userPackage addExtensionImport: extensionOnObject.
	self compile: 'userMethod: anObject ^ anObject extension' on: userClass.

	userScopedSelector := userPackage visibleSelectorForSymbol: #extension.
	
	"Send the message to create the alias"
	userClass new perform: #userMethod: with: objectClass new.
	
	"Now we remove the extension"
	extensionOnObject removeSelector: extensionScopedSelector logged: false.
	
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
	self compile: 'extension ^ 42' on: extensionOnObject.
	userPackage addExtensionImport: extensionOnObject.

	userScopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: userScopedSelector owner == userPackage.

	self assert: (objectClass new perform: userScopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test05CompileNewExtensionMethodAfterImport01InstallsExtensionInExtendedClass [

	| userScopedSelector |
	userPackage addExtensionImport: extensionOnObject.
	self compile: 'extension ^ 42' on: extensionOnObject.

	userScopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: userScopedSelector owner == userPackage.

	self assert: (objectClass new perform: userScopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test06RemoveAliasedMethod02RemovesAliases [
	"Point defines a normal method"

	| scopedSelector |
	self compile: 'extension ^ 17' on: pointClass. "We extend the superclass with the same selector"
	scopedSelector := self compile: 'extension ^ 42' on: extensionOnObject.

	pointClass removeSelector: #extension logged: false.

	self assert: (pointClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test07RemoveMethodAliasedOnDemand01RemovesAliases [

	| scopedSelector receiver |
	"We extend Point"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnPoint. "We define two methods in the hierarchy"
	self compile: 'extension ^ 42' on: stringClass.
	self compile: 'extension ^ 73' on: nicerStringClass. "We send a message to a string to alias the method." "In addition, it should automatically alias the subclass!"
	self assert: (stringClass new perform: scopedSelector) equals: 42.
	self assert: (nicerStringClass new perform: scopedSelector) equals: 73.

	stringClass removeSelector: #extension logged: false.

	receiver := stringClass new.
	self should: [ receiver perform: scopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :ex |
			self assert: ex receiver == receiver.
			self assert: ex message selector == scopedSelector ]
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test07RemoveMethodAliasedOnDemand01RemovesAliasesInSubclasses [

	| scopedSelector |
	"We extend Point"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnPoint. "We define two methods in the hierarchy"
	self compile: 'extension ^ 42' on: stringClass.
	self compile: 'extension ^ 73' on: nicerStringClass. "We send a message to a string to alias the method." "In addition, it should automatically alias the subclass!"
	self assert: (stringClass new perform: scopedSelector) equals: 42.
	self assert: (nicerStringClass new perform: scopedSelector) equals: 73.

	nicerStringClass removeSelector: #extension logged: false.

	self assert: (nicerStringClass new perform: scopedSelector) equals: 42
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test08RemoveExtensionMethodFromImport01ShouldRemoveExtensionMethodFromImporterScope [

	| scopedSelector receiver |
	"We extend Object"
	self compile: 'extension ^ 11' on: extensionOnObject. "We import it -> the method should be locally seen using the local scoped selector"
	userPackage addExtensionImport: extensionOnObject.
	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (objectClass new perform: scopedSelector) equals: 11. "We remove the extension. Now the object should not understand the message anymore"
	extensionOnObject removeSelector: (definerPackage visibleSelectorForSymbol: #extension) logged: false. "So the message is not understood anymore"
	receiver := objectClass new.
	self should: [ receiver perform: scopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: scopedSelector ]
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test08RemoveExtensionMethodFromImport02ShouldNotRemoveOriginalExtensionMethod [

	| scopedSelector receiver |
	"We extend Object"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject.

	userPackage addExtensionImport: extensionOnObject.
	self assert: (objectClass new perform: scopedSelector) equals: 11. "We remove the extension. Now the object should not understand the message anymore"
	extensionOnObject removeSelector: (definerPackage visibleSelectorForSymbol: #extension) logged: false. "So the message is not understood anymore"
	receiver := objectClass new.
	self should: [ receiver perform: scopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: scopedSelector ]
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test09RemoveImport01ShouldRemoveExtensionMethod [

	| scopedSelector receiver |
	"We extend Object"
	self compile: 'extension ^ 11' on: extensionOnObject. "We import it -> the method should be locally seen using the local scoped selector"
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
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject.

	userPackage addExtensionImport: extensionOnObject.
	self assert: (objectClass new perform: scopedSelector) equals: 11. "We remove the extension. Now the object should not understand the message anymore"
	userPackage removeExtensionImport: extensionOnObject. "So message from the extension package should still work"
	self assert: (objectClass new perform: scopedSelector) equals: 11
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test10RemoveExtensionFromSystem01ShouldRemoveExtensionMethodFromImporterScope [

	| scopedSelector receiver |
	"We extend Object"
	self compile: 'extension ^ 11' on: extensionOnObject.
	
	"We import it -> the method should be locally seen using the local scoped selector"
	userPackage addExtensionImport: extensionOnObject.
	scopedSelector := userPackage visibleSelectorForSymbol: #extension.
	self assert: (objectClass new perform: scopedSelector) equals: 11.
	
	"We remove the extension. Now the object should not understand the message anymore"
	extensionOnObject removeFromSystem: false.
	
	"So the message is not understood anymore"
	receiver := objectClass new.
	self should: [ receiver perform: scopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: scopedSelector ]
]

{ #category : 'tests' }
ExtensionHotUpdateAliasingTest >> test10RemoveExtensionFromSystem02ShouldNotRemoveOriginalExtensionMethod [

	| scopedSelector receiver |
	"We extend Object"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject.

	userPackage addExtensionImport: extensionOnObject.
	self assert: (objectClass new perform: scopedSelector) equals: 11.
	
	"We remove the extension. Now the object should not understand the message anymore"
	extensionOnObject removeFromSystem: false.
	
	"So the message is not understood anymore"
	receiver := objectClass new.
	self should: [ receiver perform: scopedSelector ] raise: MessageNotUnderstood withExceptionDo: [ :dnu |
			self assert: dnu receiver equals: receiver.
			self assert: dnu message selector equals: scopedSelector ]
]
