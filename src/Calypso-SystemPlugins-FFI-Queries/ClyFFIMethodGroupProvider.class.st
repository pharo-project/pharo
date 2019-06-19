"
I provide ""FFI calls"" method groups when given classes define them
"
Class {
	#name : #ClyFFIMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemPlugins-FFI-Queries'
}

{ #category : #'building groups' }
ClyFFIMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	^ClyMethodGroup named: 'ffi calls' on: aMethodQuery
]

{ #category : #'building groups' }
ClyFFIMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyFFIMethodQuery from: aClassScope
]
