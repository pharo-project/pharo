Class {
	#name : #ClyMethodCounterGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTestCase,
	#instVars : [
		'installedCounter'
	],
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Tests-ExecutionCounters'
}

{ #category : #running }
ClyMethodCounterGroupProviderTest >> classSampleWhichHasGroup [
	^ClyMethodCallCountersQueryTest 
]

{ #category : #running }
ClyMethodCounterGroupProviderTest >> groupProviderClass [
	^ClyMethodCounterGroupProvider
]

{ #category : #running }
ClyMethodCounterGroupProviderTest >> setUp [
	super setUp.
	
	installedCounter := ClyMethodCallCountersQueryTest new setUp; installCounterInto: #methodWithCounter
]

{ #category : #running }
ClyMethodCounterGroupProviderTest >> tearDown [
	
	installedCounter uninstall.
	
	super tearDown.
]
