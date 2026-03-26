Class {
	#name : 'ExtensionTest',
	#superclass : 'TestCase',
	#instVars : [
		'environment',
		'userClass',
		'userPackage',
		'definerPackage',
		'extensionOnObject',
		'definerPackage2',
		'objectClass',
		'pointClass',
		'extensionOnPoint'
	],
	#category : 'Kernel-CodeModel-Tests-Extensions',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Extensions'
}

{ #category : 'running' }
ExtensionTest >> setUp [

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

	extensionOnPoint := (Extension << #ExtensionOfPoint
		extendedClass: pointClass;
		installingEnvironment: environment;
		package: definerPackage) install.
	
	extensionOnObject := (Extension << #ExtensionOfObject
		extendedClass: objectClass;
		installingEnvironment: environment;
		package: definerPackage) install.

	userClass := (Object << #MyUserClass
		superclass: objectClass;
		installingEnvironment: environment;
		package: userPackage) install.
]

{ #category : 'running' }
ExtensionTest >> tearDown [
	"Make sure we did not pollute the global dictionaries"

	self deny: (Point methodDictionary keys anySatisfy: [ :k | k isKindOf: ScopedSelector ]).
	self deny: (Object methodDictionary keys anySatisfy: [ :k | k isKindOf: ScopedSelector ]).
	self deny: (Integer methodDictionary keys anySatisfy: [ :k | k isKindOf: ScopedSelector ]).
	
	environment allClassesAndTraits do: [ :e | e removeFromSystem ].
	
	super tearDown
]

{ #category : 'tests - semantic' }
ExtensionTest >> test01ExtensionKnowsExtendedClass [

	self assert: extensionOnObject extendedClass == objectClass
]

{ #category : 'tests - runtime' }
ExtensionTest >> test01NewScopedSelectorHasNoExternalUsers [

	| scopedSelector |		
	scopedSelector := definerPackage defineScopedSelector: #newSelector.
	
	self assert: scopedSelector extendedBehaviors isEmpty
]

{ #category : 'tests - semantic' }
ExtensionTest >> test02ExtensionIsPresentInEnvironment [

	self assert: (environment at: #ExtensionOfObject) == extensionOnObject
]

{ #category : 'tests - runtime' }
ExtensionTest >> test03LookupNonExtendedSelectorReturnsGlobalSelector [

	self assert: (userPackage visibleSelectorForSymbol: #maybeExtension) == #maybeExtension
]

{ #category : 'tests - runtime' }
ExtensionTest >> test04LookupExtendedSelectorInDefinerReturnsScopedSelector [

	| scopedSelector |
	"We define a extension method, which defines a scoped selector in the selector namespace"
	extensionOnObject compile: 'extensionMethod ^ 42'.

	scopedSelector := definerPackage visibleSelectorForSymbol: #extensionMethod.
	self assert: scopedSelector isScopedSelector.
	self assert: scopedSelector symbol equals: #extensionMethod.
	self assert: scopedSelector owner equals: definerPackage
]

{ #category : 'tests - runtime' }
ExtensionTest >> test05LookupExtendedSelectorInUserReturnsScopedSelector [

	| scopedSelector |
	"We define a extension method, which defines a scoped selector in the selector namespace"
	extensionOnObject compile: 'extensionMethod ^ 42'.
	userPackage addExtensionImport: extensionOnObject.

	scopedSelector := userPackage visibleSelectorForSymbol: #extensionMethod.
	self assert: scopedSelector isScopedSelector.
	self assert: scopedSelector symbol equals: #extensionMethod.
	self assert: scopedSelector owner equals: userPackage
]

{ #category : 'tests - semantic' }
ExtensionTest >> test06ExtensionsInTheSamePackageSeeEachOther [

	| receiver |
	extensionOnObject compile: 'extension2 ^ 42'.
	extensionOnObject compile: 'extension1 ^ self extension2'.

	receiver := objectClass new.
	self assert: (receiver perform: (definerPackage visibleSelectorForSymbol: #extension1)) equals: 42
]

{ #category : 'tests - semantic' }
ExtensionTest >> test07ExtensionsInTheSamePackageSeeEachOtherIfDefinedBackwards [

	| receiver |
	extensionOnObject compile: 'extension1 ^ self extension2'.
	extensionOnObject compile: 'extension2 ^ 42'.

	receiver := objectClass new.
	self assert: (receiver perform: (definerPackage visibleSelectorForSymbol: #extension1)) equals: 42
]

{ #category : 'tests - semantic' }
ExtensionTest >> test08RecursiveExtensionSeesItself [

	| receiver |
	extensionOnObject compile: 'factorial: n ^ n == 0 ifTrue: [1] ifFalse: [n * (self factorial: n - 1)]'.

	receiver := objectClass new.
	self assert: (receiver perform: (definerPackage visibleSelectorForSymbol: #factorial:) with: 5) equals: 120
]

{ #category : 'tests - semantic' }
ExtensionTest >> test09ExtensionIsNotVisibleIfNotImported [

	| receiver |
	extensionOnObject compile: 'extensionMethod ^ 42'.
	userClass compile: 'userMethod ^ self extensionMethod'.
	
	receiver := userClass new.
	[receiver perform: #userMethod. self fail]
		on: MessageNotUnderstood do: [ :dnu |
			self assert: dnu receiver == receiver.
			self assert: dnu message selector equals: #extensionMethod ]
]

{ #category : 'tests - semantic' }
ExtensionTest >> test10ExtensionIsVisibleIfImported [

	| receiver |		
	extensionOnObject compile: 'extensionMethod ^ 42'.
	userClass compile: 'userMethod ^ self extensionMethod'.

	userPackage addExtensionImport: extensionOnObject.
	
	receiver := userClass new.
	self assert: (receiver perform: #userMethod) equals: 42
]

{ #category : 'tests - semantic' }
ExtensionTest >> test11ExtensionsArePolymorphicWithBaseSelectors [

	| userInstance |
	objectClass compile: 'asString ^ ''This is not an extension'''.
	extensionOnPoint compile: 'asString ^ ''youpiii'''.
	userClass compile: 'scopedUser: anObject
		^ anObject asString'.

	userPackage addExtensionImport: extensionOnPoint.
	userInstance := userClass new.

	self assert: (userInstance perform: #scopedUser: with: pointClass new) equals: 'youpiii'.
	self assert: (userInstance perform: #scopedUser: with: objectClass new) equals: 'This is not an extension'.
		
	"Not visible if not imported"
	self assert: pointClass new asString equals: 'This is not an extension'.

]

{ #category : 'tests - runtime' }
ExtensionTest >> test12ExtensionDefinesSelectorWithExtendedClass [

	extensionOnObject compile: 'extensionMethod ^ 42'. "Now userMethod has executed, and a scoped selector for #extensionMethod has been installed in Object"
	self assert: ((definerPackage visibleSelectorForSymbol: #extensionMethod) extendedBehaviors includes: objectClass)
]

{ #category : 'tests - semantic' }
ExtensionTest >> test12ExtensionsArePolymorphicBetweenThem [

	| userInstance |
	definerPackage2 := Package named: 'Definer2'.	
	extensionOnObject compile: 'foo ^ 10'.
	extensionOnPoint compile: 'foo ^ 42'.
	userClass compile: 'scopedUser: anObject
		^ anObject foo'.

	userPackage addExtensionImport: extensionOnObject.
	userPackage addExtensionImport: extensionOnPoint.
	userInstance := userClass new.

	self assert: (userInstance perform: #scopedUser: with: pointClass new) equals: 42.
	self assert: (userInstance perform: #scopedUser: with: objectClass new) equals: 10.
]

{ #category : 'tests - semantic' }
ExtensionTest >> test13ConflictingExtensionsThrowError [

	| userInstance anotherExtensionOnObject |
	definerPackage2 := Package named: 'Definer2'.
	anotherExtensionOnObject := (Extension << #AnotherExtensionOnObject
		extendedClass: objectClass;
		installingEnvironment: environment;
		package: definerPackage2) install.
	
	extensionOnObject compile: 'foo ^ 10'.
	anotherExtensionOnObject compile: 'foo ^ 42'.
	userClass compile: 'scopedUser: anObject
		^ anObject foo'.

	userPackage addExtensionImport: extensionOnObject.
	userPackage addExtensionImport: anotherExtensionOnObject.
	userInstance := userClass new.

	self should: [(userInstance perform: #scopedUser: with: objectClass new)] raise: ExtensionConflictError with: [ :ex | self assert: ex messageText equals: 'Conflict: extension overwrites method #foo in MyObject' ].
	
]

{ #category : 'tests - runtime' }
ExtensionTest >> test13ExtensionDoesNotDefineAliasBeforeSend [

	| userInstance scopedSelector |
	extensionOnPoint compile: 'asString ^ ''youpiii'''.
	userClass compile: 'scopedUser: anObject
		^ anObject asString'.

	userPackage addExtensionImport: extensionOnPoint.
	userInstance := userClass new.

	scopedSelector := userPackage visibleSelectorForSymbol: #asString.
	self assert: scopedSelector isScopedSelector.
	self assert: (objectClass lookupSelector: scopedSelector) isNil
]

{ #category : 'tests - runtime' }
ExtensionTest >> test14DefinerAndUserSelectorAreNotTheSame [

	| definerScopedSelector userScopedSelector |
	extensionOnPoint compile: 'asString ^ ''youpiii'''.

	userPackage addExtensionImport: extensionOnPoint.

	definerScopedSelector := definerPackage visibleSelectorForSymbol: #asString.
	userScopedSelector := userPackage visibleSelectorForSymbol: #asString.

	self deny: definerScopedSelector == userScopedSelector
]

{ #category : 'tests - semantic' }
ExtensionTest >> test14ExtensionSuperSends [

	| userInstance |

	extensionOnObject compile: 'foo ^ 10'.
	extensionOnPoint compile: 'foo ^ super foo + 1'.
	userClass compile: 'scopedUser: anObject
		^ anObject foo'.

	userPackage addExtensionImport: extensionOnObject.
	userPackage addExtensionImport: extensionOnPoint.
	userInstance := userClass new.

	self assert: (userInstance perform: #scopedUser: with: pointClass new) equals: 11.
	self assert: (userInstance perform: #scopedUser: with: objectClass new) equals: 10.
]

{ #category : 'tests - semantic' }
ExtensionTest >> test15ExtensionsInHierarchySubclassBindingFirst [

	| userInstance |
	extensionOnObject compile: 'foo ^ 10'.
	extensionOnPoint compile: 'foo ^ 42'.
	userClass compile: 'scopedUser: anObject
		^ anObject foo'.

	userPackage addExtensionImport: extensionOnObject.
	userPackage addExtensionImport: extensionOnPoint.
	userInstance := userClass new.

	self assert: (userInstance perform: #scopedUser: with: pointClass new) equals: 42.
	self assert: (userInstance perform: #scopedUser: with: objectClass new) equals: 10.
]

{ #category : 'tests - semantic' }
ExtensionTest >> test15ExtensionsInHierarchySuperclassBindingFirst [

	| userInstance |
	extensionOnObject compile: 'foo ^ 10'.
	extensionOnPoint compile: 'foo ^ 42'.
	userClass compile: 'scopedUser: anObject
		^ anObject foo'.

	userPackage addExtensionImport: extensionOnObject.
	userPackage addExtensionImport: extensionOnPoint.
	userInstance := userClass new.

	self assert: (userInstance perform: #scopedUser: with: objectClass new) equals: 10.
	self assert: (userInstance perform: #scopedUser: with: pointClass new) equals: 42.
]

{ #category : 'tests - semantic' }
ExtensionTest >> test15ExtensionsWithIsolatedSuperSends [

	| userInstance extensionOnPoint2 superSenderSelector pointObject |
	definerPackage2 := Package named: 'Definer2'.
	extensionOnPoint2 := ((Extension << #ExtensionOfInteger)
		                      extendedClass: pointClass;
		                      installingEnvironment: environment;
		                      package: definerPackage2) install.

	extensionOnObject compile: 'foo ^ 10'.
	superSenderSelector := extensionOnPoint2 compile: 'foo ^ super foo + 1'.
	userClass compile: 'scopedUser: anObject
		^ anObject foo'.

	userPackage addExtensionImport: extensionOnObject.
	userPackage addExtensionImport: extensionOnPoint2.
	userInstance := userClass new.

	self assert: (userInstance perform: #scopedUser: with: objectClass new) equals: 10.

	pointObject := pointClass new.
	self should: [ userInstance perform: #scopedUser: with: pointObject ] raise: MessageNotUnderstood with: [ :dnu |
			self assert: dnu receiver equals: pointObject.
			self assert: dnu message selector equals: (extensionOnPoint2 package visibleSelectorForSymbol: #foo). "I don't like the sender sender sender"
			self assert: dnu signalerContext sender sender sender method == (pointClass >> superSenderSelector) ]
]

{ #category : 'tests - runtime' }
ExtensionTest >> test15LookupAliasedSelectorInUserReturnsExtendedClass [

	| definerScopedSelector userScopedSelector extendedClass1 extendedClass2 |
	extensionOnPoint compile: 'asString ^ ''youpiii'''.

	userPackage addExtensionImport: extensionOnPoint.

	definerScopedSelector := definerPackage visibleSelectorForSymbol: #asString.
	userScopedSelector := userPackage visibleSelectorForSymbol: #asString.
	extendedClass1 := userScopedSelector lookupAliasedMethodOnReceiverClass: Point.
	extendedClass2 := definerScopedSelector lookupAliasedMethodOnReceiverClass: Point.

	self assert: extendedClass1 == extendedClass2
]

{ #category : 'tests - runtime' }
ExtensionTest >> test16ExtensionDoesNotTrackAliasedClassBeforeSend [

	| userInstance scopedSelector |
	extensionOnPoint compile: 'asString ^ ''youpiii'''.
	userClass compile: 'scopedUser: anObject
		^ anObject asString'.

	userPackage addExtensionImport: extensionOnPoint.
	userInstance := userClass new.

	scopedSelector := userPackage visibleSelectorForSymbol: #asString.
	self deny: (scopedSelector extendedBehaviors includes: pointClass)
]

{ #category : 'tests - runtime' }
ExtensionTest >> test17ExtensionSelectorDoesNotDefineAlias [

	| userInstance definerScopedSelector |
	objectClass compile: 'asString ^ ''this is not an extension'''.
	extensionOnPoint compile: 'asString ^ ''youpiii'''.
	userClass compile: 'scopedUser: anObject
		^ anObject asString'.

	userPackage addExtensionImport: extensionOnPoint.
	userInstance := userClass new. "This sends a scoped message to a non extended class.
	The lookup will find the global selector and alias it to the scoped one."
	self assert: (userInstance perform: #scopedUser: with: objectClass new) equals: 'this is not an extension'.

	definerScopedSelector := definerPackage visibleSelectorForSymbol: #asString.

	self assert: definerScopedSelector extendedBehaviors asArray equals: { pointClass }.
	self assert: definerScopedSelector aliasedBehaviors isEmpty
]

{ #category : 'tests - runtime' }
ExtensionTest >> test18CleanExtendedBehaviorsCleansExtensionMethods [

	| scopedSelector |
	scopedSelector := extensionOnObject compile: 'extensionMethod ^ 42'.
	self assert: scopedSelector isScopedSelector.

	self assert: (objectClass new perform: scopedSelector) equals: 42.
	
	definerPackage cleanupExtendedBehaviors.
	
	self deny: (objectClass new respondsTo: scopedSelector)
]

{ #category : 'tests - runtime' }
ExtensionTest >> test18ScopedSelectorTracksAliasedClassAfterSend [

	| userInstance userScopedSelector |
	objectClass compile: 'asString ^ ''this is not an extension'''.
	extensionOnPoint compile: 'asString ^ ''youpiii'''.
	userClass compile: 'scopedUser: anObject
		^ anObject asString'.

	userPackage addExtensionImport: extensionOnPoint.
	userInstance := userClass new.
	"This sends a scoped message to a non extended class.
	The lookup will find the global selector and alias it to the scoped one."
	self assert: (userInstance perform: #scopedUser: with: objectClass new) equals: 'this is not an extension'.

	userScopedSelector := userPackage visibleSelectorForSymbol: #asString.

	self assert: userScopedSelector extendedBehaviors isEmpty.
	self assertCollection: userScopedSelector aliasedBehaviors asArray hasSameElements: {
			objectClass.
			pointClass }
]

{ #category : 'tests - runtime' }
ExtensionTest >> test19CleanExtendedBehaviorsRemovesTrackedBehaviors [

	| receiver scopedSelector |
	extensionOnObject compile: 'extensionMethod ^ 42'.
	userClass compile: 'userMethod ^ self extensionMethod'.

	userPackage addExtensionImport: extensionOnObject.

	receiver := userClass new.
	receiver perform: #userMethod.

	scopedSelector := definerPackage visibleSelectorForSymbol: #extensionMethod.
	definerPackage cleanupExtendedBehaviors.

	self assert: scopedSelector extendedBehaviors isEmpty
]

{ #category : 'tests - runtime' }
ExtensionTest >> test20CleanExtendedBehaviorsCleansAliases [

	| userInstance scopedSelector |
	objectClass compile: 'asString ^ ''this is not an extension'''.
	extensionOnPoint compile: 'asString ^ ''youpiii'''.
	userClass compile: 'scopedUser: anObject
		^ anObject asString'.

	userPackage addExtensionImport: extensionOnPoint.
	userInstance := userClass new.

	self assert: (userInstance perform: #scopedUser: with: objectClass new) equals: 'this is not an extension'.

	scopedSelector := userPackage visibleSelectorForSymbol: #asString.

	userPackage cleanupExtendedBehaviors.

	self deny: (scopedSelector extendedBehaviors includes: objectClass)
]
