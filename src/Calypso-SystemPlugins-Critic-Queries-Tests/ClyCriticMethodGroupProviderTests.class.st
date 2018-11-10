Class {
	#name : #ClyCriticMethodGroupProviderTests,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-Critic-Queries-Tests'
}

{ #category : #running }
ClyCriticMethodGroupProviderTests >> classSampleWhichHasGroup [

	^ClyClassWithProblemMethods 
]

{ #category : #running }
ClyCriticMethodGroupProviderTests >> groupProviderClass [
	^ClyCriticMethodGroupProvider 
]
