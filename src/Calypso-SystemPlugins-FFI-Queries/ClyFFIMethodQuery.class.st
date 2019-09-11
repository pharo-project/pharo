"
I return all FFI methods from my scope
"
Class {
	#name : #ClyFFIMethodQuery,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-FFI-Queries'
}

{ #category : #printing }
ClyFFIMethodQuery >> description [
	^'ffi calls'
]

{ #category : #testing }
ClyFFIMethodQuery >> selectsMethod: aMethod [
	^ aMethod isFFIMethod
]
