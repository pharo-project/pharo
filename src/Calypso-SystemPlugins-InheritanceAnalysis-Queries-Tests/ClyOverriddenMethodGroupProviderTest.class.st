Class {
	#name : #ClyOverriddenMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTest,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyOverriddenMethodGroupProviderTest >> classSampleWhichHasGroup [

	^ClyAbstractClassMock
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
