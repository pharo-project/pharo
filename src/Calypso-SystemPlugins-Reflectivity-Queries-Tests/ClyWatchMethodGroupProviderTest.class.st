Class {
	#name : #ClyWatchMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTest,
	#instVars : [
		'installedWatchpoint'
	],
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Tests-Watchpoints'
}

{ #category : #running }
ClyWatchMethodGroupProviderTest >> classSampleWhichHasGroup [
	^ClyMethodWatchQueryTest
]

{ #category : #running }
ClyWatchMethodGroupProviderTest >> groupProviderClass [
	^ClyWatchMethodGroupProvider
]

{ #category : #running }
ClyWatchMethodGroupProviderTest >> setUp [
	super setUp.
	
	installedWatchpoint := ClyMethodWatchQueryTest new setUp; installWatchInto: #methodWithWatch 
]

{ #category : #running }
ClyWatchMethodGroupProviderTest >> tearDown [
	
	installedWatchpoint uninstall.
	
	super tearDown.
]
