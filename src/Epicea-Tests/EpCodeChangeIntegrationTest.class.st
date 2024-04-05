Class {
	#name : 'EpCodeChangeIntegrationTest',
	#superclass : 'EpEnabledIntegrationTest',
	#instVars : [
		'aClass'
	],
	#category : 'Epicea-Tests-Integration',
	#package : 'Epicea-Tests',
	#tag : 'Integration'
}

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> packageNameForTesting [

	^ #'EpiceaTestingWith-A-Really-WeirdPackageName'
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> packageTagNameForTesting [

	^ 'WeirdPackageTagName'
]

{ #category : 'running' }
EpCodeChangeIntegrationTest >> tearDown [

	self packageOrganizer removePackage: self packageNameForTesting.
	super tearDown
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testBehaviorCommentChange [

	aClass := classFactory newClass.
	self assert: (self countLogEventsWith: EpBehaviorCommentChange) equals: 0.

	aClass comment: 'hey!' stamp: DateAndTime now.
	self assert: (self countLogEventsWith: EpBehaviorCommentChange) equals: 1
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testBehaviorCommentChangeWithoutStamp [

	| newClass |
	self assert: (self countLogEventsWith: EpBehaviorCommentChange) equals: 0.

	newClass := classFactory newClass.
	newClass comment: 'This is a comment'.
	self assert: (self countLogEventsWith: EpBehaviorCommentChange) equals: 1
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testBehaviorNameChange [

	| newClass |
	self assert: (self countLogEventsWith: EpBehaviorNameChange) equals: 0.

	newClass := classFactory newClass.
	newClass rename: #AnotherClassNameForTesting1.
	self assert: (self countLogEventsWith: EpBehaviorNameChange) equals: 1
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testClassAddition [

	self assert: (self countLogEventsWith: EpClassAddition) equals: 0.

	classFactory newClass.
	self assert: (self countLogEventsWith: EpClassAddition) equals: 1
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testClassInstanceVariableAddition [

	aClass := classFactory newClass.
	self assert: (self countLogEventsWith: EpClassModification) equals: 0.

	aClass class addInstVarNamed: #x.
	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
	self assert: (self allLogEventsWith: EpClassModification) first hasMetaclassChanges
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testClassInstanceVariableRemoval [

	aClass := classFactory newClass.
	aClass class addInstVarNamed: #x.
	self assert: (self countLogEventsWith: EpClassModification) equals: 1.

	aClass class removeInstVarNamed: #x.
	self assert: (self countLogEventsWith: EpClassModification) equals: 2.
	self assert: ((self allLogEventsWith: EpClassModification) allSatisfy: #hasMetaclassChanges)
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testClassModificationOfTraitComposition [

	| aTrait |
	aClass := classFactory newClass.
	aTrait := classFactory newTrait.
	aClass setTraitComposition: aTrait asTraitComposition.

	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
	self assert: (self countLogEventsWith: EpMethodAddition) equals: 0.
	self assert: (self countLogEventsWith: EpMethodModification) equals: 0
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testClassModificationWithHierarchy [

	| modifications |
	aClass := classFactory newClass.
	classFactory make: [ :aBuilder | aBuilder superclass: aClass ].
	aClass addSlot: #i asSlot.

	modifications := (self allLogEntriesWith: EpClassModification) collect: [ :each | each content ].
	self assert: modifications size equals: 1.
	self assert: modifications first behaviorAffectedName equals: aClass name
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testClassRemoval [

	| className events |
	aClass := classFactory newClass.
	className := aClass name.
	aClass removeFromSystem.

	events := self allLogEventsWith: EpClassRemoval.
	self assert: events size equals: 1.
	"It shouldn't begin with AnObsolete"
	self assert: events anyOne behaviorAffectedName equals: className
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testClassRemovalWithMethods [

	| events package |
	aClass := classFactory newClass.
	package := aClass package name.
	1 to: 7 do: [ :index |
		aClass compile: 'inst' , index asString.
		aClass classSide compile: 'class' , index asString ].
	aClass removeFromSystem.

	events := self allLogEventsWith: EpMethodRemoval.
	self assert: events size equals: 14.
	self assert: (events allSatisfy: [ :each | each affectedPackageName = package ])
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testInstanceVariableAddition [

	aClass := classFactory newClass.
	self assert: (self countLogEventsWith: EpClassModification) equals: 0.

	aClass addInstVarNamed: #x.
	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
	self assert: (self allLogEventsWith: EpClassModification) first hasNonMetaclassChanges
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testInstanceVariableRemoval [

	aClass := classFactory make: [ :aBuilder | aBuilder slots: #(x) ].

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
	aClass removeInstVarNamed: #x.

	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
	self assert: (self allLogEventsWith: EpClassModification) first hasNonMetaclassChanges
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testMetaclassInstanceVariableAddition [

	aClass := classFactory newClass.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
	aClass classSide instanceVariableNames: 'x'.
	self assert: (self countLogEventsWith: EpClassModification) equals: 1
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testMetaclassInstanceVariableAdditionWithTrait [

	| aTrait |
	aClass := classFactory newClass.
	aTrait := classFactory newTrait.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
	aClass classSide trait: aTrait asTraitComposition slots: #( x ).
	self assert: (self countLogEventsWith: EpClassModification) equals: 1
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testMethodAddition [

	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.

	self assert: (self countLogEventsWith: EpMethodAddition) equals: 1
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testMethodModificationOfProtocol [

	| events event |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'old'.
	aClass >> #fortyTwo protocol: 'new'.

	events := self allLogEventsWith: EpMethodModification.
	self assert: events size equals: 1.
	event := events anyOne.
	self assert: event oldMethod protocol equals: 'old'.
	self assert: event newMethod protocol equals: 'new'
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testMethodModificationOfSourceCode [

	| events event |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	aClass compile: 'fortyTwo ^nil'.

	events := self allLogEventsWith: EpMethodModification.
	self assert: events size equals: 1.
	event := events anyOne.
	self assert: event oldMethod sourceCode equals: 'fortyTwo ^42'.
	self assert: event newMethod sourceCode equals: 'fortyTwo ^nil'
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testMethodModificationWithWideString [

	aClass := classFactory newClass.
	aClass compile: self wideStringSourceCode.
	aClass compile: self wideStringSourceCode, ' '.

	self assert: (self countLogEventsWith: EpMethodModification) equals: 1
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testMethodProtocolChange [

	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: #accessing.
	aClass compile: 'fortyTwo ^42' classified: #testing.

	self assert: (self countLogEventsWith: EpMethodModification) equals: 2
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testMethodRecompilationShouldNotLog [

	| headBeforeRecompiling |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	headBeforeRecompiling := monitor log head.
	aClass compile: 'fortyTwo ^42'.

	self assert: monitor log head identicalTo: headBeforeRecompiling
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testPackageAddition [

	self assert: (self countLogEventsWith: EpPackageAddition) equals: 0.

	self packageOrganizer addPackage: self packageNameForTesting.

	self assert: (self countLogEventsWith: EpPackageAddition) equals: 1.
	self assert: (self allLogEventsWith: EpPackageAddition) first packageName equals: self packageNameForTesting
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testPackageRemoval [

	self packageOrganizer addPackage: self packageNameForTesting.

	self assert: (self countLogEventsWith: EpPackageRemoval) equals: 0.

	self packageOrganizer removePackage: self packageNameForTesting.

	self assert: (self countLogEventsWith: EpPackageRemoval) equals: 1.
	self assert: (self allLogEventsWith: EpPackageRemoval) first packageName equals: self packageNameForTesting
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testPackageTagAddition [

	self assert: (self countLogEventsWith: EpPackageTagAddition) equals: 0.

	self packageOrganizer ensureTag: self packageTagNameForTesting inPackage: self packageNameForTesting.

	self assert: (self countLogEventsWith: EpPackageTagAddition) equals: 1.
	self assert: (self allLogEventsWith: EpPackageTagAddition) first packageName equals: self packageNameForTesting.
	self assert: (self allLogEventsWith: EpPackageTagAddition) first tagName equals: self packageTagNameForTesting
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testPackageTagRemoval [

	self packageOrganizer ensureTag: self packageTagNameForTesting inPackage: self packageNameForTesting.
	self assert: (self countLogEventsWith: EpPackageTagRemoval) equals: 0.

	self packageOrganizer removeTag: self packageTagNameForTesting fromPackage: self packageNameForTesting.

	self assert: (self countLogEventsWith: EpPackageTagRemoval) equals: 1.
	self assert: (self allLogEventsWith: EpPackageTagRemoval) first packageName equals: self packageNameForTesting.
	self assert: (self allLogEventsWith: EpPackageTagRemoval) first tagName equals: self packageTagNameForTesting
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testProtocolAddition [

	aClass := classFactory newClass.
	aClass addProtocol: #testing42.

	self assert: (self countLogEventsWith: EpProtocolAddition) equals: 1
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testProtocolRemoval [

	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: #testing42.
	aClass removeProtocol: #testing42.

	self assert: (self countLogEventsWith: EpProtocolRemoval) equals: 1
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testProtocolRemovalOfUnclassifiedProtocol [
	"This protocol is not relevant then it's not logged."

	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: Protocol unclassified.
	aClass removeProtocol: Protocol unclassified.

	self assert: (self countLogEventsWith: EpProtocolRemoval) equals: 0
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testRedundantClassDefinition [

	aClass := classFactory newClass.
	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
	(self class compiler evaluate: aClass definitionString) fluidInstall.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
	(self class compiler evaluate: aClass classSide definitionString) fluidInstall.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testRemoveTraitFromComposition [

	aClass := classFactory make: [ :aBuilder | aBuilder traitComposition: TSizeTest ].
	self
		shouldnt: [
			self class classInstaller make: [ :aBuilder |
				aBuilder
					name: aClass name;
					traitComposition: {  };
					package: aClass packageName ] ]
		raise: MessageNotUnderstood
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testTraitAddition [

	| trait events event package |
	trait := classFactory newTrait.
	package := trait package name.

	events := self allLogEventsWith: EpTraitAddition.
	self assert: events size equals: 1.
	event := events anyOne.
	self assert: event behaviorAffected package name equals: package
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testTraitModificationOfTraitComposition [

	| aTrait |
	aClass := classFactory newTrait.
	aTrait := classFactory newTrait.
	aClass setTraitComposition: aTrait asTraitComposition.

	self assert: (self countLogEventsWith: EpTraitModification) equals: 1.
	self assert: (self countLogEventsWith: EpMethodAddition) equals: 0.
	self assert: (self countLogEventsWith: EpMethodModification) equals: 0
]

{ #category : 'tests' }
EpCodeChangeIntegrationTest >> testTraitRemoval [

	| trait package events event |
	trait := classFactory newTrait.
	package := trait package name.
	trait removeFromSystem.

	events := self allLogEventsWith: EpTraitRemoval.
	self assert: events size equals: 1.
	event := events anyOne.
	self assert: event behaviorAffected package equals: package
]
