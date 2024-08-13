Class {
	#name : 'ReSmalllintTest',
	#superclass : 'TestCase',
	#category : 'General-Rules-Tests-Migrated',
	#package : 'General-Rules-Tests',
	#tag : 'Migrated'
}

{ #category : 'accessing' }
ReSmalllintTest class >> packageNamesUnderTest [
	^ #('Refactoring-Critics')
]

{ #category : 'private' }
ReSmalllintTest >> assertEnvironment: anEnvironment hasAllClassesOfTheResult: critiques [

	critiques do: [ :crit |
		| target |
		target := crit entity.
		self assert: target isClass.
		self assert: (anEnvironment includesClass: target) ]
]

{ #category : 'private' }
ReSmalllintTest >> assertEnvironment: anEnvironment hasAllMethodsOfTheResult: critiques [

	critiques do: [ :crit |
		| target |
		target := crit entity.
		self assert: target isCompiledMethod.
		self assert: (anEnvironment includesMethod: target) ]
]

{ #category : 'private' }
ReSmalllintTest >> assertIsValidRule: aRule [

	self assert: (aRule name isString and: [ aRule name isNotEmpty ]) description: 'Missing rule name'.
	self assert: (aRule group isString and: [ aRule group isNotEmpty ]) description: 'Missing group name'.
	self assert: (aRule rationale isString and: [ aRule rationale isNotEmpty ]) description: 'Missing rationale'.
	self assert: (#( error warning information ) includes: aRule severity) description: 'Invalid severity'
]

{ #category : 'asserting' }
ReSmalllintTest >> assertResult: critiques hasAllClassesInEnvironment: anEnvironment [
	anEnvironment classesDo: [ :class |
		self assert: (critiques anySatisfy: [ :crit |
			crit entity instanceSide = class instanceSide ]) ]
]

{ #category : 'asserting' }
ReSmalllintTest >> assertResult: critiques hasAllMethodsInEnvironment: anEnvironment [

	| missingMethods |
	missingMethods := OrderedCollection new.
	anEnvironment methodsDo: [ :method | (critiques anySatisfy: [ :crit | crit entity = method ]) ifFalse: [ missingMethods add: method ] ].

	"We collect the missing methods instead of doing an assert in the previous block to give a better feedback in the CI and test failure."
	self assertEmpty: missingMethods
]

{ #category : 'private' }
ReSmalllintTest >> assertResultOfRule: critiques hasSameClassesAsEnvironment: anEnvironment [
	self assertResult: critiques hasAllClassesInEnvironment: anEnvironment.
	self assertEnvironment: anEnvironment hasAllClassesOfTheResult: critiques
]

{ #category : 'private' }
ReSmalllintTest >> assertResultOfRule: critiques hasSameSelectorsAsEnvironment: anEnvironment [
	self assertResult: critiques hasAllMethodsInEnvironment: anEnvironment.
	self assertEnvironment: anEnvironment hasAllMethodsOfTheResult: critiques
]

{ #category : 'private' }
ReSmalllintTest >> classAndMetaClassRuleFor: aTestSelector [
	self classAndMetaClassRuleFor: aTestSelector plusClasses: #()
]

{ #category : 'private' }
ReSmalllintTest >> classAndMetaClassRuleFor: aTestSelector plusClasses: aCollectionOfTestedClasses [
	| result expectedResultClasses expectedResultEnvironment |
	result := self
		runRuleFor: aTestSelector
		onEnvironment: self classEnvironmentWithAllObjectClasses.
	expectedResultClasses :=
		(aCollectionOfTestedClasses copyWith: (self objectClassFor: aTestSelector)).
	expectedResultEnvironment :=
		self classEnvironmentWithAll: expectedResultClasses.
	expectedResultClasses do: [ :each |
		expectedResultEnvironment addClass: each classSide ].
	self
		assertResultOfRule: result
		hasSameClassesAsEnvironment: expectedResultEnvironment
]

{ #category : 'private' }
ReSmalllintTest >> classEnvironmentWith: aClass [
	^ self classEnvironmentWithAll: (Array with: aClass)
]

{ #category : 'private' }
ReSmalllintTest >> classEnvironmentWithAll: aClassCollection [
	| classEnvironment |
	classEnvironment := RBClassEnvironment new.
	aClassCollection do: [ :each | classEnvironment addClass: each ].
	^ classEnvironment
]

{ #category : 'private' }
ReSmalllintTest >> classEnvironmentWithAllObjectClasses [
	^ self classEnvironmentWithAll: RBSmalllintTestSuperclassObject withAllSubclasses
]

{ #category : 'private' }
ReSmalllintTest >> classNamed: aSymbol [
	^ self class environment
		at: aSymbol
		ifAbsent: [ self error: aSymbol asString , ' class not found' ]
]

{ #category : 'private' }
ReSmalllintTest >> classRuleFor: aTestSelector [
	^ self classRuleFor: aTestSelector plusClasses: #()
]

{ #category : 'private' }
ReSmalllintTest >> classRuleFor: aTestSelector plusClasses: aCollectionOfTestedClasses [
	| result expectedResultEnvironment |
	result := self
		runRuleFor: aTestSelector
		onEnvironment: self classEnvironmentWithAllObjectClasses.
	expectedResultEnvironment :=
		self classEnvironmentWithAll:
			(aCollectionOfTestedClasses copyWith:
				(self objectClassFor: aTestSelector)).
	self
		assertResultOfRule: result
		hasSameClassesAsEnvironment: expectedResultEnvironment
]

{ #category : 'private' }
ReSmalllintTest >> currentSelector [
	^ testSelector
]

{ #category : 'private' }
ReSmalllintTest >> objectClassFor: aTestSelector [
	^ self classNamed:
		('RBSmalllintTest',
		(aTestSelector asString allButFirst: 4 "'test' size"),
		'Object') asSymbol
]

{ #category : 'private' }
ReSmalllintTest >> objectClassSelectorFor: aTestSelector [
	^ (aTestSelector asString allButFirst: 4) "'test' size"
		uncapitalized asSymbol
]

{ #category : 'private' }
ReSmalllintTest >> ruleClassFor: aTestSelector [
	^ self classNamed:
		('Re',
		(aTestSelector asString allButFirst: 4), "'test' size"
		'Rule') asSymbol
]

{ #category : 'private' }
ReSmalllintTest >> ruleFor: aTestSelector [
	self ruleFor: aTestSelector plusSelectors: #()
]

{ #category : 'private' }
ReSmalllintTest >> ruleFor: aTestSelector plusSelectors: aCollectionOfTestedSelectors [
	| result expectedResultEnvironment |
	result := self
		runRuleFor: aTestSelector
		onEnvironment: (self classEnvironmentWith: RBSmalllintTestObject).
	expectedResultEnvironment := RBSelectorEnvironment new.
	(aCollectionOfTestedSelectors
		copyWith: (self objectClassSelectorFor: aTestSelector))
		do: [ :each |
			expectedResultEnvironment addClass: RBSmalllintTestObject selector: each.
			self
				assert: (RBSmalllintTestObject includesSelector: each)
				description: 'Missing test method RBSmalllintTestObject>>' , each printString ].
	self
		assertResultOfRule: result
		hasSameSelectorsAsEnvironment: expectedResultEnvironment
]

{ #category : 'private' }
ReSmalllintTest >> runRule: aRule onEnvironment: anEnvironment [
	| runner |
	self assertIsValidRule: aRule.
	runner := ReSmalllintChecker new.
	runner
		rule: {aRule};
		environment: anEnvironment;
		run.
	^ runner
]

{ #category : 'private' }
ReSmalllintTest >> runRuleFor: aTestSelector onEnvironment: anEnvironment [
	| rule runner |
	rule := (self ruleClassFor: aTestSelector) new.
	self assertIsValidRule: rule.
	runner := ReSmalllintChecker new.
	runner
		rule: {rule};
		environment: anEnvironment;
		run.
	^ runner criticsOf: rule
]

{ #category : 'private' }
ReSmalllintTest >> testAddClassRules [

	| checker |
	
	checker := ReSmalllintChecker new 
		classRules: (ReAbstractRule allSubclasses collect: #new);
		yourself.

	self denyEmpty: checker classRules

]

{ #category : 'tests' }
ReSmalllintTest >> testAddRemoveDependents [
	self classRuleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testAsOrderedCollectionNotNeeded [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testAssignmentInBlock [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testAssignmentInIfTrue [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testAssignmentWithoutEffect [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testAtIfAbsent [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testBadMessage [
	self ruleFor: self currentSelector plusSelectors: #(badMessage collectionMessagesToExternalObject debuggingMessageSent)
]

{ #category : 'tests' }
ReSmalllintTest >> testBooleanPrecedence [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testClassNotReferenced [
	self classAndMetaClassRuleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testCodeCruftLeftInMethods [
	self ruleFor: self currentSelector plusSelectors: #(haltClassMentioned flagged transcriptMentioned debuggingMessageSent)
]

{ #category : 'tests' }
ReSmalllintTest >> testCollectSelectNotUsed [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testCollectionAtCollectionSize [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testCollectionProtocol [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testDefinesEqualNotHash [
	self classRuleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testDetectContains [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testEmptyExceptionHandler [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testEndTrueFalse [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testEqualNotUsed [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testEqualsTrue [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testEquivalentSuperclassMethods [
	^ self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testExtraBlock [
	self
		ruleFor: self currentSelector
		plusSelectors: #(#testMethod1 )
]

{ #category : 'tests' }
ReSmalllintTest >> testFileBlocks [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testFloatEqualityComparison [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testGuardingClause [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testJustSendsSuper [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testLiteralArrayCharacters [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testLiteralArrayContainsComma [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testLocalMethodsSameThanTrait [
	self classAndMetaClassRuleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testLongMethods [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testMinMax [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testModifiesCollection [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testMultiplePeriodsTerminatingStatement [
	self
		ruleFor: self currentSelector
		plusSelectors: #(multiplePeriodsTerminatingAssignmentStatement)
]

{ #category : 'tests' }
ReSmalllintTest >> testNoClassComment [
	self classAndMetaClassRuleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testNotOptimizedIf [
	self ruleFor: self currentSelector plusSelectors: {#ifTrueBlocks}
]

{ #category : 'tests' }
ReSmalllintTest >> testPrecedence [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testRBMissingSubclassResponsibilityRule [

	| superClass subClass1 subClass2 rule |
	rule := ReMissingSubclassResponsibilityRule new.

	superClass := self class classInstaller make: [ :aClassBuilder |
		              aClassBuilder
			              name: 'MySuperclass';
			              package: self class package name ].

	subClass1 := self class classInstaller make: [ :aClassBuilder |
		             aClassBuilder
			             name: 'MySubclass1';
			             superclass: superClass;
			             package: self class package name ].

	subClass2 := self class classInstaller make: [ :aClassBuilder |
		             aClassBuilder
			             name: 'MySubclass2';
			             superclass: superClass;
			             package: self class package name ].

	subClass1 compile: 'method ^ 3'.
	subClass2 compile: 'method ^ 5'.

	self denyEmpty: (rule check: superClass).

	superClass compile: 'method ^ 2'.

	self assertEmpty: (rule check: superClass).

	superClass compile: 'method ^self subclassResponsibility'.

	self assertEmpty: (rule check: superClass).

	subClass1 removeFromSystem.
	subClass2 removeFromSystem.
	superClass removeFromSystem
]

{ #category : 'tests' }
ReSmalllintTest >> testRefersToClass [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testReturnInEnsure [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testReturnsBooleanAndOther [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testReturnsIfTrue [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testSearchingLiteral [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testSendsDifferentSuper [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testShouldntRaiseError [
	self
		ruleFor: self currentSelector
		plusSelectors: #(shouldntRaiseException)
]

{ #category : 'tests' }
ReSmalllintTest >> testSizeCheck [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testStringConcatenation [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testTempVarOverridesInstVar [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testTempsReadBeforeWritten [
	self ruleFor: self currentSelector
		plusSelectors: #(#inlineTemporary #noMoveDefinition #tempsReadBeforeWritten #equalNotUsed #fileBlocks #referencesConditionFor:)
]

{ #category : 'tests' }
ReSmalllintTest >> testThreeElementPoint [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testToDoCollect [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testToDoWithIncrement [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testUnaryAccessingMethodWithoutReturn [
	self
		ruleFor: self currentSelector
		plusSelectors:
			#(#unaryAccessingMessageStatementMethodWithoutReturn
				#unaryAccessingBranchingStatementMethodWithoutReturn)
]

{ #category : 'tests' }
ReSmalllintTest >> testUnclassifiedMethods [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testUncommonMessageSend [
	self ruleFor: self currentSelector plusSelectors: {#uncommonMessageSend2}
]

{ #category : 'tests - unique identifier' }
ReSmalllintTest >> testUniqueIdentifierName [

	| idToRules |

	idToRules := Dictionary new.
	RBLintRule allSubclasses do: [ :ruleClass |
		(idToRules at: ruleClass uniqueIdentifierName ifAbsentPut: [ Set new ]) add: ruleClass ].
	self assert: (idToRules noneSatisfy: [ :ruleClasses | ruleClasses size > 1 ])
]

{ #category : 'tests' }
ReSmalllintTest >> testUnoptimizedAndOr [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testUsesAdd [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testUsesTrue [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testWhileTrue [
	self ruleFor: self currentSelector
]

{ #category : 'tests' }
ReSmalllintTest >> testYourselfNotUsed [
	self ruleFor: self currentSelector
]
