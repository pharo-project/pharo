Class {
	#name : #ClyCriticMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-Critic-Queries-Tests'
}

{ #category : #running }
ClyCriticMethodGroupProviderTest >> classSampleWhichHasGroup [

	^ClyClassWithProblemMethods 
]

{ #category : #running }
ClyCriticMethodGroupProviderTest >> groupProviderClass [
	^ClyCriticMethodGroupProvider 
]
