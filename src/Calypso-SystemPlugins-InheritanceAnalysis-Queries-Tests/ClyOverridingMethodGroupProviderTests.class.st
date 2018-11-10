Class {
	#name : #ClyOverridingMethodGroupProviderTests,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyOverridingMethodGroupProviderTests >> classSampleWhichHasGroup [

	^ClyClassWhichInheritsAbstractClass 
]

{ #category : #running }
ClyOverridingMethodGroupProviderTests >> groupProviderClass [
	^ClyOverridingMethodGroupProvider 
]

{ #category : #running }
ClyOverridingMethodGroupProviderTests >> setUpEnvironment [
	super setUpEnvironment.
	
	environment addPlugin: ClyInheritanceAnalysisEnvironmentPlugin new
]
