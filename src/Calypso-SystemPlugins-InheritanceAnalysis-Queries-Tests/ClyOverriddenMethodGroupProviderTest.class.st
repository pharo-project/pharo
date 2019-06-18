Class {
	#name : #ClyOverriddenMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyOverriddenMethodGroupProviderTest >> classSampleWhichHasGroup [

	^ClyAbstractClassExample
]

{ #category : #running }
ClyOverriddenMethodGroupProviderTest >> groupProviderClass [
	^ClyOverriddenMethodGroupProvider 
]

{ #category : #running }
ClyOverriddenMethodGroupProviderTest >> setUpEnvironment [
	super setUpEnvironment.
	
	environment addPlugin: ClyInheritanceAnalysisEnvironmentPlugin new
]
