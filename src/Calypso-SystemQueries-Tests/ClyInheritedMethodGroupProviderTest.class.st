Class {
	#name : 'ClyInheritedMethodGroupProviderTest',
	#superclass : 'ClyMethodGroupProviderTest',
	#category : 'Calypso-SystemQueries-Tests-Domain',
	#package : 'Calypso-SystemQueries-Tests',
	#tag : 'Domain'
}

{ #category : 'running' }
ClyInheritedMethodGroupProviderTest >> classSampleWhichHasGroup [
	^ClyClass1FromP1Mock
]

{ #category : 'running' }
ClyInheritedMethodGroupProviderTest >> groupProviderClass [
	^ClyInheritedMethodGroupProvider
]
