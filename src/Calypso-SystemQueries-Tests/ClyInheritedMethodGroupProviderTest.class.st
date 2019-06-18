Class {
	#name : #ClyInheritedMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyInheritedMethodGroupProviderTest >> classSampleWhichHasGroup [
	^ClyClass1FromP1 
]

{ #category : #running }
ClyInheritedMethodGroupProviderTest >> groupProviderClass [
	^ClyInheritedMethodGroupProvider
]
