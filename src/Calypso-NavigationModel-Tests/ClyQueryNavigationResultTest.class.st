Class {
	#name : 'ClyQueryNavigationResultTest',
	#superclass : 'ClyAbstractNavigationEnvironmentTest',
	#instVars : [
		'queryResult',
		'query'
	],
	#category : 'Calypso-NavigationModel-Tests',
	#package : 'Calypso-NavigationModel-Tests'
}

{ #category : 'testing' }
ClyQueryNavigationResultTest class >> isAbstract [
	^self = ClyQueryNavigationResultTest
]

{ #category : 'running' }
ClyQueryNavigationResultTest >> createQuery [

	^ClyReturningScopeBasisExampleQuery
		from: (ClyExampleScope emptyIn: environment)
]

{ #category : 'running' }
ClyQueryNavigationResultTest >> createQueryResult [
	self subclassResponsibility
]

{ #category : 'running' }
ClyQueryNavigationResultTest >> setUp [
	super setUp.

	query := self createQuery.

	queryResult := self createQueryResult prepareNewFor: query in: environment.
	queryResult cancelRebuild.
	"by default result is in state to be rebuilt by query.
	We do not need it in tests where we usually just test fillWith: logic"
]

{ #category : 'tests' }
ClyQueryNavigationResultTest >> testCheckingForEmpty [
	self
		assert: queryResult isEmpty;
		deny: queryResult notEmpty
]

{ #category : 'tests' }
ClyQueryNavigationResultTest >> testComparisonWithAnotherKindOfResult [
	self deny: queryResult equals: ClyQueryResult new
]

{ #category : 'tests' }
ClyQueryNavigationResultTest >> testComparisonWithCopy [

	| queryResult2 |
	queryResult2 := queryResult copy.

	self assert: queryResult equals: queryResult2
]

{ #category : 'tests' }
ClyQueryNavigationResultTest >> testComparisonWithItself [

	self assert: queryResult equals: queryResult
]

{ #category : 'tests' }
ClyQueryNavigationResultTest >> testComparisonWithSimilarResult [

	| queryResult2 |
	queryResult := self createQueryResult.
	queryResult2 := self createQueryResult.

	self assert: queryResult equals: queryResult2.
	self assert: queryResult hash equals: queryResult2 hash
]

{ #category : 'tests' }
ClyQueryNavigationResultTest >> testForceLazyRebuild [

	self assert: queryResult items isNotNil.

	queryResult forceLazyRebuild.

	self assert: queryResult needsRebuild
]

{ #category : 'tests' }
ClyQueryNavigationResultTest >> testIsBuiltWhenItemsExist [

	queryResult initializeItems.

	self assert: queryResult isBuilt
]

{ #category : 'tests' }
ClyQueryNavigationResultTest >> testItemsChangedNotificationShouldResetItems [

	queryResult initializeItems.
	self assert: queryResult isBuilt.

	queryResult itemsChanged.

	self deny: queryResult isBuilt.
	self assert: queryResult items isNil
]

{ #category : 'tests' }
ClyQueryNavigationResultTest >> testNeedsRebuildByDefault [

	queryResult := self createQueryResult.

	self assert: queryResult needsRebuild
]

{ #category : 'tests' }
ClyQueryNavigationResultTest >> testPreparation [
	| newQuery newResult |
	newQuery := self createQuery.
	newResult := queryResult prepareNewFor: newQuery in: environment.

	self assert: newResult class equals: queryResult class.
	self deny: newResult identicalTo: queryResult.
	self assert: newResult buildingQuery identicalTo: newQuery.
	self assert: newResult environment equals: environment.
	self assert: newResult isProtected
]
