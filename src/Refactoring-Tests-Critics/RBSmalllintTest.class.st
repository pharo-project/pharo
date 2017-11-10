Class {
	#name : #RBSmalllintTest,
	#superclass : #TestCase,
	#category : #Refactoring-Tests-Critics
}

{ #category : #accessing }
RBSmalllintTest class >> packageNamesUnderTest [
	^ #('Refactoring-Critics')
]

{ #category : #private }
RBSmalllintTest >> assertEnvironment: superEnvironment hasAllClassesInEnvironment: subEnvironment [
	subEnvironment classesDo: [ :class |
		self assert: (superEnvironment includesClass: class) ]
]

{ #category : #private }
RBSmalllintTest >> assertEnvironment: superEnvironment hasAllSelectorsInEnvironment: subEnvironment [
	subEnvironment classesDo: [ :class | 
		(subEnvironment selectorsForClass: class) 
			do: [ :selector |
				self assert: (superEnvironment includesSelector: selector in: class) ] ]
]

{ #category : #private }
RBSmalllintTest >> assertIsValidRule: aRule [
	self
		assert: (aRule name isString and: [ aRule name notEmpty ])
		description: 'Missing rule name'.
	self
		assert: (aRule group isString and: [ aRule group notEmpty ])
		description: 'Missing group name'.
	self
		assert: (aRule rationale isString and: [ aRule rationale notEmpty ])
		description: 'Missing rationale'.
	self
		assert: (#(error warning information) includes: aRule severity)
		description: 'Invalid severity'
]

{ #category : #private }
RBSmalllintTest >> assertResultOfRule: aLintRule hasSameClassesAsEnvironment: anEnvironment [
	| resultEnvironment |
	resultEnvironment := aLintRule result.
	self assertEnvironment: resultEnvironment hasAllClassesInEnvironment: anEnvironment.
	self assertEnvironment: anEnvironment hasAllClassesInEnvironment: resultEnvironment
]

{ #category : #private }
RBSmalllintTest >> assertResultOfRule: aLintRule hasSameSelectorsAsEnvironment: anEnvironment [
	| resultEnvironment |
	resultEnvironment := aLintRule result.
	self assertEnvironment: resultEnvironment hasAllSelectorsInEnvironment: anEnvironment.
	self assertEnvironment: anEnvironment hasAllSelectorsInEnvironment: resultEnvironment
]

{ #category : #private }
RBSmalllintTest >> classAndMetaClassRuleFor: aTestSelector [
	self classAndMetaClassRuleFor: aTestSelector plusClasses: #()
]

{ #category : #private }
RBSmalllintTest >> classAndMetaClassRuleFor: aTestSelector plusClasses: aCollectionOfTestedClasses [
	| rule expectedResultClasses expectedResultEnvironment |
	rule := self
		runRuleFor: aTestSelector
		onEnvironment: self classEnvironmentWithAllObjectClasses.
	expectedResultClasses :=
		(aCollectionOfTestedClasses copyWith: (self objectClassFor: aTestSelector)).
	expectedResultEnvironment :=
		self classEnvironmentWithAll: expectedResultClasses.
	expectedResultClasses do: [ :each |
		expectedResultEnvironment addClass: each theMetaClass ].
	self
		assertResultOfRule: rule
		hasSameClassesAsEnvironment: expectedResultEnvironment
]

{ #category : #private }
RBSmalllintTest >> classEnvironmentWith: aClass [
	^ self classEnvironmentWithAll: (Array with: aClass)
]

{ #category : #private }
RBSmalllintTest >> classEnvironmentWithAll: aClassCollection [
	| classEnvironment |
	classEnvironment := RBClassEnvironment new.
	aClassCollection do: [ :each | classEnvironment addClass: each ].
	^ classEnvironment
]

{ #category : #private }
RBSmalllintTest >> classEnvironmentWithAllObjectClasses [
	^ self classEnvironmentWithAll: RBSmalllintTestSuperclassObject withAllSubclasses
]

{ #category : #private }
RBSmalllintTest >> classNamed: aSymbol [
	^ self class environment
		at: aSymbol
		ifAbsent: [ self error: aSymbol asString , ' class not found' ]
]

{ #category : #private }
RBSmalllintTest >> classRuleFor: aTestSelector [
	^ self classRuleFor: aTestSelector plusClasses: #()
]

{ #category : #private }
RBSmalllintTest >> classRuleFor: aTestSelector plusClasses: aCollectionOfTestedClasses [
	| rule expectedResultEnvironment |
	rule := self
		runRuleFor: aTestSelector
		onEnvironment: self classEnvironmentWithAllObjectClasses.
	expectedResultEnvironment :=
		self classEnvironmentWithAll:
			(aCollectionOfTestedClasses copyWith:
				(self objectClassFor: aTestSelector)).
	self
		assertResultOfRule: rule
		hasSameClassesAsEnvironment: expectedResultEnvironment
]

{ #category : #private }
RBSmalllintTest >> currentSelector [
	^ testSelector
]

{ #category : #private }
RBSmalllintTest >> objectClassFor: aTestSelector [
	^ self classNamed:
		('RBSmalllintTest',
		(aTestSelector asString allButFirst: 4 "'test' size"),
		'Object') asSymbol
]

{ #category : #private }
RBSmalllintTest >> objectClassSelectorFor: aTestSelector [
	^ (aTestSelector asString allButFirst: 4) "'test' size"
		uncapitalized asSymbol
]

{ #category : #private }
RBSmalllintTest >> ruleClassFor: aTestSelector [
	^ self classNamed:
		('RB',
		(aTestSelector asString allButFirst: 4), "'test' size"
		'Rule') asSymbol.
]

{ #category : #private }
RBSmalllintTest >> ruleFor: aTestSelector [
	self ruleFor: aTestSelector plusSelectors: #()
]

{ #category : #private }
RBSmalllintTest >> ruleFor: aTestSelector plusSelectors: aCollectionOfTestedSelectors [
	| rule expectedResultEnvironment |
	rule := self
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
		assertResultOfRule: rule
		hasSameSelectorsAsEnvironment: expectedResultEnvironment
]

{ #category : #private }
RBSmalllintTest >> runRuleFor: aTestSelector onEnvironment: anEnvironment [
	| rule |
	rule := (self ruleClassFor: aTestSelector) new.
	self assertIsValidRule: rule.
	RBSmalllintChecker
		runRule: rule
		onEnvironment: anEnvironment.
	^ rule.
]

{ #category : #tests }
RBSmalllintTest >> testAddRemoveDependents [
	self classRuleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testAsOrderedCollectionNotNeeded [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testAssignmentInBlock [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testAssignmentInIfTrue [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testAssignmentWithoutEffect [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testAtIfAbsent [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testBadMessage [
	self ruleFor: self currentSelector plusSelectors: #(badMessage collectionMessagesToExternalObject)
]

{ #category : #tests }
RBSmalllintTest >> testBooleanPrecedence [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testClassNotReferenced [
	self classAndMetaClassRuleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testCodeCruftLeftInMethods [
	self ruleFor: self currentSelector plusSelectors: #(haltClassMentioned flagged transcriptMentioned debbuggingMessageSent)
]

{ #category : #tests }
RBSmalllintTest >> testCollectSelectNotUsed [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testCollectionMessagesToExternalObject [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testCollectionProtocol [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testContains [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testDefinesEqualNotHash [
	self classRuleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testDetectContains [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testEmptyExceptionHandler [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testEndTrueFalse [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testEqualNotUsed [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testEqualsTrue [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testEquivalentSuperclassMethods [
	^ self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testExtraBlock [
	self 
		ruleFor: self currentSelector
		plusSelectors: #(#testMethod1 )
]

{ #category : #tests }
RBSmalllintTest >> testFileBlocks [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testFloatEqualityComparison [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testFullBlocks [
	"skip this test in squeak"

"	self blockRuleFor: self currentSelector
		plusSelectors: #(#caller1 #fullBlocks #detectContains #fileBlocks #moveDefinition #caller #assignmentInBlock #equalNotUsed #stringConcatenation #noMoveDefinition #referencesConditionFor: #returnInEnsure)"
]

{ #category : #tests }
RBSmalllintTest >> testGuardingClause [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testIfTrueReturns [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testJustSendsSuper [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testLiteralArrayCharacters [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testLiteralArrayContainsComma [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testLongMethods [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testMinMax [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testModifiesCollection [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testMultiplePeriodsTerminatingStatement [
	self
		ruleFor: self currentSelector
		plusSelectors: #(multiplePeriodsTerminatingAssignmentStatement)
]

{ #category : #tests }
RBSmalllintTest >> testNoClassComment [
	self classAndMetaClassRuleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testPrecedence [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testRBMissingSubclassResponsibilityRule [
	| sup sub1 sub2 |
	sup := Object subclass: 'MySuperclass' instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: self class category.
	sub1 := sup subclass: 'MySubclass1' instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: self class category.
	sub2 := sup subclass: 'MySubclass2' instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: self class category.
	
	sub1 compile: 'method ^ 3'.
	sub2 compile: 'method ^ 5'.
	
	self deny: (RBMissingSubclassResponsibilityRule new
		runOnEnvironment: (RBClassEnvironment class: sup);
		result) isEmpty.
	
	sup  compile: 'method ^ 2'.
		
	self assert: (RBMissingSubclassResponsibilityRule new
		runOnEnvironment: (RBClassEnvironment class: sup);
		result) isEmpty.
		
	sup  compile: 'method ^self subclassResponsibility'.
		
	self assert: (RBMissingSubclassResponsibilityRule new
		runOnEnvironment: (RBClassEnvironment class: sup);
		result) isEmpty.
		
	sub1 removeFromSystem.
	sub2 removeFromSystem.
	sup  removeFromSystem
]

{ #category : #tests }
RBSmalllintTest >> testRefersToClass [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testReturnInEnsure [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testReturnsBooleanAndOther [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testReturnsIfTrue [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testSearchingLiteral [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testSendsDifferentSuper [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testShouldntRaiseError [
	self
		ruleFor: self currentSelector
		plusSelectors: #(shouldntRaiseException)
]

{ #category : #tests }
RBSmalllintTest >> testSizeCheck [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testStringConcatenation [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testTempVarOverridesInstVar [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testTempsReadBeforeWritten [
	self ruleFor: self currentSelector
		plusSelectors: #(#inlineTemporary #noMoveDefinition #tempsReadBeforeWritten #equalNotUsed #fileBlocks #referencesConditionFor:)
]

{ #category : #tests }
RBSmalllintTest >> testThreeElementPoint [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testToDo [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testToDoCollect [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testToDoWithIncrement [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testUnaryAccessingMethodWithoutReturn [
	self
		ruleFor: self currentSelector
		plusSelectors:
			#(#unaryAccessingMessageStatementMethodWithoutReturn
				#unaryAccessingBranchingStatementMethodWithoutReturn)
]

{ #category : #tests }
RBSmalllintTest >> testUnclassifiedMethods [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testUncommonMessageSend [
	self ruleFor: self currentSelector plusSelectors: {#uncommonMessageSend2}
]

{ #category : #'test unique identifier' }
RBSmalllintTest >> testUniqueIdentifierName [

	| idToRules |

	idToRules := Dictionary new.
	RBLintRule allSubclasses do: [ :ruleClass | 
		(idToRules at: ruleClass uniqueIdentifierName ifAbsentPut: [ Set new ]) add: ruleClass ].
	self assert: (idToRules noneSatisfy: [ :ruleClasses | ruleClasses size > 1 ]).

]

{ #category : #tests }
RBSmalllintTest >> testUnoptimizedAndOr [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testUsesAdd [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testUsesTrue [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testWhileTrue [
	self ruleFor: self currentSelector
]

{ #category : #tests }
RBSmalllintTest >> testYourselfNotUsed [
	self ruleFor: self currentSelector
]
