Class {
	#name : #ClyFlagMethodGroupProviderTests,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-Flags-Queries-Tests'
}

{ #category : #running }
ClyFlagMethodGroupProviderTests >> classSampleWhichHasGroup [
	^ClyClassWithFlags
]

{ #category : #running }
ClyFlagMethodGroupProviderTests >> groupProviderClass [
	^ClyFlagMethodGroupProvider 
]
