Class {
	#name : #ClyRequiredMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTest,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyRequiredMethodGroupProviderTest >> classSampleWhichHasGroup [

	^ClyClassWhichInheritsAbstractSubclassMock
]

{ #category : #running }
ClyRequiredMethodGroupProviderTest >> groupProviderClass [
	^ClyRequiredMethodGroupProvider 
]
