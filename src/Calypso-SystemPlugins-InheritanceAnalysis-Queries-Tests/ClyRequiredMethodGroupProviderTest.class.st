Class {
	#name : #ClyRequiredMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyRequiredMethodGroupProviderTest >> classSampleWhichHasGroup [

	^ClyClassWhichInheritsAbstractSubclass
]

{ #category : #running }
ClyRequiredMethodGroupProviderTest >> groupProviderClass [
	^ClyRequiredMethodGroupProvider 
]
