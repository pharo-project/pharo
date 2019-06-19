Class {
	#name : #ClyExpectedFailureMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTestCase,
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
