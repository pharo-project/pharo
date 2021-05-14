Class {
	#name : #ClyInheritedMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTest,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyInheritedMethodGroupProviderTest >> classSampleWhichHasGroup [
	^ClyClass1FromP1Mock 
]

{ #category : #running }
ClyInheritedMethodGroupProviderTest >> groupProviderClass [
	^ClyInheritedMethodGroupProvider
]
