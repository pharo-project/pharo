"
I am special plugin which analyzes ffi methods and mark them with ClyFFIMethodTag.
Also I provide ffi method group to see all ffi calls together
"
Class {
	#name : #ClyFFIEnvironmentPlugin,
	#superclass : #ClySystemEnvironmentPlugin,
	#category : #'Calypso-SystemPlugins-FFI-Queries'
}

{ #category : #'method groups' }
ClyFFIEnvironmentPlugin >> collectMethodGroupProviders [ 
	^{ClyFFIMethodGroupProvider new}
]

{ #category : #'item decoration' }
ClyFFIEnvironmentPlugin >> decorateBrowserItem: anItem ofMethod: aMethod [

	aMethod isFFIMethod ifTrue: [ 
		anItem markWith: ClyFFIMethodTag]
]
