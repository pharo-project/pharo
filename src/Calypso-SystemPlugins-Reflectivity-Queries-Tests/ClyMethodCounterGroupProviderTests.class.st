Class {
	#name : #ClyMethodCounterGroupProviderTests,
	#superclass : #ClyMethodGroupProviderTestCase,
	#instVars : [
		'installedCounter'
	],
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Tests-ExecutionCounters'
}

{ #category : #running }
ClyMethodCounterGroupProviderTests >> classSampleWhichHasGroup [
	^ClyMethodCallCountersTests 
]

{ #category : #running }
ClyMethodCounterGroupProviderTests >> groupProviderClass [
	^ClyMethodCounterGroupProvider
]

{ #category : #running }
ClyMethodCounterGroupProviderTests >> setUp [
	super setUp.
	
	installedCounter := ClyMethodCallCountersTests new setUp; installCounterInto: #methodWithCounter
]

{ #category : #running }
ClyMethodCounterGroupProviderTests >> tearDown [
	
	installedCounter uninstall.
	
	super tearDown.
]
