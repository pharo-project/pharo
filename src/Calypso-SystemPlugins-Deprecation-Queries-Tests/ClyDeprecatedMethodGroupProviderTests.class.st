Class {
	#name : #ClyDeprecatedMethodGroupProviderTests,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-Deprecation-Queries-Tests'
}

{ #category : #running }
ClyDeprecatedMethodGroupProviderTests >> classSampleWhichHasGroup [

	^ClyClassWithDeprecatedMethods 
]

{ #category : #running }
ClyDeprecatedMethodGroupProviderTests >> groupProviderClass [
	^ClyDeprecatedMethodGroupProvider
]
