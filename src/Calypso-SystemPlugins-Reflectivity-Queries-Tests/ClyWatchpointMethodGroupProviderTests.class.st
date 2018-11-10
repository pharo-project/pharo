Class {
	#name : #ClyWatchpointMethodGroupProviderTests,
	#superclass : #ClyMethodGroupProviderTestCase,
	#instVars : [
		'installedWatchpoint'
	],
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Tests-Watchpoints'
}

{ #category : #running }
ClyWatchpointMethodGroupProviderTests >> classSampleWhichHasGroup [
	^ClyMethodWatchpointsTests
]

{ #category : #running }
ClyWatchpointMethodGroupProviderTests >> groupProviderClass [
	^ClyWatchpointMethodGroupProvider
]

{ #category : #running }
ClyWatchpointMethodGroupProviderTests >> setUp [
	super setUp.
	
	installedWatchpoint := ClyMethodWatchpointsTests new setUp; installWatchpointInto: #methodWithWatchpoint 
]

{ #category : #running }
ClyWatchpointMethodGroupProviderTests >> tearDown [
	
	installedWatchpoint uninstall.
	
	super tearDown.
]
