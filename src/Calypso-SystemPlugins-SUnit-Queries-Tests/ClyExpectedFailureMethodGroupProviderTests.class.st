Class {
	#name : #ClyExpectedFailureMethodGroupProviderTests,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-SUnit-Queries-Tests'
}

{ #category : #running }
ClyExpectedFailureMethodGroupProviderTests >> classSampleWhichHasGroup [
	^ClyTestedClassTests
]

{ #category : #running }
ClyExpectedFailureMethodGroupProviderTests >> groupProviderClass [
	^ClyExpectedFailureMethodGroupProvider 
]
