Class {
	#name : #ClyRequiredMethodGroupProviderTests,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyRequiredMethodGroupProviderTests >> classSampleWhichHasGroup [

	^ClyClassWhichInheritsAbstractSubclass
]

{ #category : #running }
ClyRequiredMethodGroupProviderTests >> groupProviderClass [
	^ClyRequiredMethodGroupProvider 
]
