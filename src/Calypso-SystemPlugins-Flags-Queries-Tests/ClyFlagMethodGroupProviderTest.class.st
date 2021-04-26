Class {
	#name : #ClyFlagMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTest,
	#category : #'Calypso-SystemPlugins-Flags-Queries-Tests'
}

{ #category : #running }
ClyFlagMethodGroupProviderTest >> classSampleWhichHasGroup [
	^ClyClassWithFlags
]

{ #category : #running }
ClyFlagMethodGroupProviderTest >> groupProviderClass [
	^ClyFlagMethodGroupProvider 
]
