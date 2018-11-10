Class {
	#name : #ClyUndeclaredMethodGroupProviderTests,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-Undeclared-Queries-Tests'
}

{ #category : #running }
ClyUndeclaredMethodGroupProviderTests >> classSampleWhichHasGroup [

	^ClyClassWithUndeclares
]

{ #category : #running }
ClyUndeclaredMethodGroupProviderTests >> groupProviderClass [
	^ClyUndeclaredMethodGroupProvider
]
