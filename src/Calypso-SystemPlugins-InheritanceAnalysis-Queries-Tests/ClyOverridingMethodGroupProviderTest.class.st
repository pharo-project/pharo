Class {
	#name : #ClyOverridingMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyOverridingMethodGroupProviderTest >> classSampleWhichHasGroup [

	^ClyClassWhichInheritsAbstractClass 
]

{ #category : #running }
ClyOverridingMethodGroupProviderTest >> groupProviderClass [
	^ClyOverridingMethodGroupProvider 
]

{ #category : #running }
ClyOverridingMethodGroupProviderTest >> setUpEnvironment [
	super setUpEnvironment.
	
	environment addPlugin: ClyInheritanceAnalysisEnvironmentPlugin new
]
