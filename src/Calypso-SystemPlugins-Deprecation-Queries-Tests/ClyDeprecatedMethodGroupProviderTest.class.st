Class {
	#name : #ClyDeprecatedMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTest,
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
