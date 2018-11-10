Class {
	#name : #ClyInheritedMethodGroupProviderTests,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyInheritedMethodGroupProviderTests >> classSampleWhichHasGroup [
	^ClyClass1FromP1 
]

{ #category : #running }
ClyInheritedMethodGroupProviderTests >> groupProviderClass [
	^ClyInheritedMethodGroupProvider
]
