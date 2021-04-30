Class {
	#name : #ClyOverridingMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTest,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyOverridingMethodGroupProviderTest >> classSampleWhichHasGroup [

	^ClyClassWhichInheritsAbstractClassMock 
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
