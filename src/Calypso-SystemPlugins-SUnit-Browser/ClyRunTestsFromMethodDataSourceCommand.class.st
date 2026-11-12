"
I am a root of commands hierarchy which supposed to run all method available in particular method data source.

My subclasses are used in ClyQueryBrowser to run all tests available in the list.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	methodDataSource:		<ClyDataSource>
"
Class {
	#name : 'ClyRunTestsFromMethodDataSourceCommand',
	#superclass : 'ClyTestCommand',
	#instVars : [
		'methodDataSource'
	],
	#category : 'Calypso-SystemPlugins-SUnit-Browser-Commands',
	#package : 'Calypso-SystemPlugins-SUnit-Browser',
	#tag : 'Commands'
}

{ #category : 'testing' }
ClyRunTestsFromMethodDataSourceCommand class >> isAbstract [
	^self = ClyRunTestsFromMethodDataSourceCommand
]

{ #category : 'execution' }
ClyRunTestsFromMethodDataSourceCommand >> buildTestSuite [

	self subclassResponsibility
]

{ #category : 'testing' }
ClyRunTestsFromMethodDataSourceCommand >> canBeExecuted [

	^ self context browser resultView dataSource hasMetaProperty: ClyTestResultProperty
]

{ #category : 'execution' }
ClyRunTestsFromMethodDataSourceCommand >> executeRefactoring [
	| testSuite result |
	testSuite := self buildTestSuite.

	result := testSuite run.

	result updateResultsInHistory.

	self
		notifyUserAboutResults: result
		with: 'Tests complete'
]

{ #category : 'testing' }
ClyRunTestsFromMethodDataSourceCommand >> hasTestResult [

	^self methodDataSource hasMetaProperty: ClyTestResultProperty
]

{ #category : 'accessing' }
ClyRunTestsFromMethodDataSourceCommand >> methodDataSource [
	^ methodDataSource ifNil: [ self context ifNotNil: [ :ctx | ctx browser resultView dataSource ] ]
]

{ #category : 'execution' }
ClyRunTestsFromMethodDataSourceCommand >> testMethodsDo: aBlock [

	self methodDataSource queryResult rawItemsDo: [ :eachMethod |
		(eachMethod isTestMethod and: [ eachMethod methodClass isAbstract not])
			ifTrue: [ aBlock value: eachMethod]]
]

{ #category : 'testing' }
ClyRunTestsFromMethodDataSourceCommand >> testResult [

	^self methodDataSource getMetaProperty: ClyTestResultProperty
]
