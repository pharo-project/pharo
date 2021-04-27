Class {
	#name : #ClyUndeclaredMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTest,
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
