Class {
	#name : #ClyExpectedFailureMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTest,
	#category : #'Calypso-SystemPlugins-SUnit-Queries-Tests'
}

{ #category : #running }
ClyExpectedFailureMethodGroupProviderTest >> classSampleWhichHasGroup [
	^ClyTestedClassTest
]

{ #category : #running }
ClyExpectedFailureMethodGroupProviderTest >> groupProviderClass [
	^ClyExpectedFailureMethodGroupProvider 
]
