Class {
	#name : #ClyUndeclaredMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-Undeclared-Queries-Tests'
}

{ #category : #running }
ClyUndeclaredMethodGroupProviderTest >> classSampleWhichHasGroup [

	^ClyClassWithUndeclares
]

{ #category : #running }
ClyUndeclaredMethodGroupProviderTest >> groupProviderClass [
	^ClyUndeclaredMethodGroupProvider
]
