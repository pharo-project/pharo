Class {
	#name : #ClyWatchpointMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTestCase,
	#instVars : [
		'installedWatchpoint'
	],
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Tests-Watchpoints'
}

{ #category : #running }
ClyWatchpointMethodGroupProviderTest >> classSampleWhichHasGroup [
	^ClyMethodWatchpointsQueryTest
]

{ #category : #running }
ClyWatchpointMethodGroupProviderTest >> groupProviderClass [
	^ClyWatchpointMethodGroupProvider
]

{ #category : #running }
ClyWatchpointMethodGroupProviderTest >> setUp [
	super setUp.
	
	installedWatchpoint := ClyMethodWatchpointsQueryTest new setUp; installWatchpointInto: #methodWithWatchpoint 
]

{ #category : #running }
ClyWatchpointMethodGroupProviderTest >> tearDown [
	
	installedWatchpoint uninstall.
	
	super tearDown.
]
