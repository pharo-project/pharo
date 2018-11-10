Class {
	#name : #ClyOverriddenMethodGroupProviderTests,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyOverriddenMethodGroupProviderTests >> classSampleWhichHasGroup [

	^ClyAbstractClassExample
]

{ #category : #running }
ClyOverriddenMethodGroupProviderTests >> groupProviderClass [
	^ClyOverriddenMethodGroupProvider 
]

{ #category : #running }
ClyOverriddenMethodGroupProviderTests >> setUpEnvironment [
	super setUpEnvironment.
	
	environment addPlugin: ClyInheritanceAnalysisEnvironmentPlugin new
]
