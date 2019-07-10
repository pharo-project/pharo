Class {
	#name : #ClyFFIMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-FFI-Queries-Tests'
}

{ #category : #running }
ClyFFIMethodGroupProviderTest >> classSampleWhichHasGroup [
	^ClyClassWithFFIMethods 
]

{ #category : #running }
ClyFFIMethodGroupProviderTest >> groupProviderClass [
	^ClyFFIMethodGroupProvider 
]
