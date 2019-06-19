Class {
	#name : #ClyDeprecatedMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-Deprecation-Queries-Tests'
}

{ #category : #running }
ClyDeprecatedMethodGroupProviderTest >> classSampleWhichHasGroup [

	^ClyClassWithDeprecatedMethods 
]

{ #category : #running }
ClyDeprecatedMethodGroupProviderTest >> groupProviderClass [
	^ClyDeprecatedMethodGroupProvider
]
