"
I am example class which has ffi methods
"
Class {
	#name : #ClyClassWithFFIMethods,
	#superclass : #Object,
	#category : #'Calypso-SystemPlugins-FFI-Queries-Tests'
}

{ #category : #'ffi methods' }
ClyClassWithFFIMethods >> ffiMethod [

	^ self ffiCall: #(int system #(char * command))
]

{ #category : #'not ffi methods' }
ClyClassWithFFIMethods >> notFFIMethod [
]
