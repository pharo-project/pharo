"
I am a FFICompilerPlugin. I am a plugin for the compiler that make the compiled method store the arguments names to be used for FFI when the sources are not loaded or unloaded.

See also FFIAdditionalFFIMethodState and FDBDecompiler>>createNArgs:
"
Class {
	#name : #FFICompilerPlugin,
	#superclass : #OCCompilerASTPlugin,
	#classVars : [
		'FFICalloutSelectors'
	],
	#category : #UnifiedFFI
}

{ #category : #accessing }
FFICompilerPlugin class >> priority [
	^ 2
]

{ #category : #accessing }
FFICompilerPlugin >> ffiCalloutSelectors [
	^FFICalloutSelectors ifNil: [ FFICalloutSelectors := IdentitySet withAll: #( 
        ffiCall: ffiCall:module: ffiCall:options: ffiCall:module:options: 
        nbCall: nbCall:module: nbCall:options: nbCall:module:options:)]
]

{ #category : #api }
FFICompilerPlugin >> transform [
	"While plugin is used to compile every method we can use it for two purposes:
		- collect all ffiCallout selectors like ffiCall:, nbCall:
		- mark ffi methods which isses collected selectors that they are FFI"
	self transformsFFICalloutTranslator ifTrue: [ 
		self ffiCalloutSelectors add: ast selector. ^ast ].

	self transformsFFIMethod ifFalse: [ ^ast].
	 
	ast ensureCachedArgumentNames.
	ast methodPropertyAt: #isFFIMethod put: true.
	^ast
]

{ #category : #api }
FFICompilerPlugin >> transformsFFICalloutTranslator [

   ^ast hasPragmaNamed: #ffiCalloutTranslator
]

{ #category : #api }
FFICompilerPlugin >> transformsFFIMethod [

 	ast nodesDo: [:each | 
      each isMessage ifTrue: [    
    		(self ffiCalloutSelectors includes: each selector) ifTrue: [^true]]].
  ^false
]
