Class {
	#name : #ClyFFIMethodGroupProviderTests,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemPlugins-FFI-Queries-Tests'
}

{ #category : #running }
ClyFFIMethodGroupProviderTests >> classSampleWhichHasGroup [
	^ClyClassWithFFIMethods 
]

{ #category : #running }
ClyFFIMethodGroupProviderTests >> groupProviderClass [
	^ClyFFIMethodGroupProvider 
]
