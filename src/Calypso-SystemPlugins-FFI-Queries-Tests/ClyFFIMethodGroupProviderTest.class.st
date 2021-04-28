Class {
	#name : #ClyFFIMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTest,
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
