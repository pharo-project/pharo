"
I am a root of commands hierarchy which supposed to run all method available in particular method data source.

My subclasses are used in ClyQueryBrowser to run all tests available in the list.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	methodDataSource:		<ClyDataSource>
"
Class {
	#name : #ClyRunTestsFromMethodDataSourceCommand,
	#superclass : #ClyTestCommand,
	#instVars : [
		'methodDataSource'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Commands'
}

{ #category : #testing }
ClyRunTestsFromMethodDataSourceCommand class >> canBeExecutedInContext: aBrowserContext [

	^aBrowserContext browser resultView dataSource hasMetaProperty: ClyTestResultProperty 
]

{ #category : #testing }
ClyRunTestsFromMethodDataSourceCommand class >> isAbstract [
	^self = ClyRunTestsFromMethodDataSourceCommand 
]

{ #category : #activation }
ClyRunTestsFromMethodDataSourceCommand class >> menuOrder [
	<classAnnotationDependency>
	^10000
]

{ #category : #activation }
ClyRunTestsFromMethodDataSourceCommand class >> queryBrowserToolbarActivation [
	<classAnnotation>
	
	^ClyToolbarActivation 
		byItemOf: ClyButtonToolbarGroup order: self menuOrder for: ClyQueryBrowserContext
]

{ #category : #execution }
ClyRunTestsFromMethodDataSourceCommand >> buildTestSuite [
	
	self subclassResponsibility 
]

{ #category : #execution }
ClyRunTestsFromMethodDataSourceCommand >> execute [
	| testSuite result |
	testSuite := self buildTestSuite.
	
	result := testSuite run.
	
	result updateResultsInHistory.
	
	self
		notifyUserAboutResults: result 
		with: 'Tests complete'
]

{ #category : #testing }
ClyRunTestsFromMethodDataSourceCommand >> hasTestResult [

	^methodDataSource hasMetaProperty: ClyTestResultProperty
]

{ #category : #execution }
ClyRunTestsFromMethodDataSourceCommand >> readParametersFromContext: aBrowserContext [
	super readParametersFromContext: aBrowserContext.
	
	methodDataSource := aBrowserContext browser resultView dataSource
]

{ #category : #execution }
ClyRunTestsFromMethodDataSourceCommand >> testMethodsDo: aBlock [

	methodDataSource queryResult rawItemsDo: [ :eachMethod | 
		(eachMethod isTestMethod and: [ eachMethod methodClass isAbstract not]) 
			ifTrue: [ aBlock value: eachMethod]]
]

{ #category : #testing }
ClyRunTestsFromMethodDataSourceCommand >> testResult [

	^methodDataSource getMetaProperty: ClyTestResultProperty
]
