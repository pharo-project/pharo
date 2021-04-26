Class {
	#name : #ClyExpectedFailureMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTest,
	#category : #'Calypso-SystemPlugins-SUnit-Queries-Tests'
}

{ #category : #running }
ClyExpectedFailureMethodGroupProviderTest >> classSampleWhichHasGroup [
	^ClyTestedClassMockTest
]

{ #category : #running }
ClyExpectedFailureMethodGroupProviderTest >> groupProviderClass [
	^ClyExpectedFailureMethodGroupProvider 
]
