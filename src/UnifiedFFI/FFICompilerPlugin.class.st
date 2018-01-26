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
	#classInstVars : [
		'collector'
	],
	#category : #UnifiedFFI
}

{ #category : #adding }
FFICompilerPlugin class >> addAFfiCalloutSelectorEvent: aPragmaAdded [
	self addAFfiCalloutSelectorFromPragma: aPragmaAdded pragma
]

{ #category : #adding }
FFICompilerPlugin class >> addAFfiCalloutSelectorFromPragma: aPragma [
	self addFfiCalloutSelector: aPragma methodSelector.
	self recompileSendersOf: aPragma method
]

{ #category : #adding }
FFICompilerPlugin class >> addFfiCalloutSelector: aString [ 
	FFICalloutSelectors add: aString
]

{ #category : #accessing }
FFICompilerPlugin class >> defaultFfiCalloutSelectors [
	^ #(ffiCall: #ffiCall:module: #ffiCall:options: #ffiCall:module:options: nbCall: #nbCall:module: #nbCall:options: #nbCall:module:options:)
]

{ #category : #'class initialization' }
FFICompilerPlugin class >> initialize [
	FFICalloutSelectors := IdentitySet
		withAll: self defaultFfiCalloutSelectors.
	self initializeFfiCalloutSelectorsListUpdate
]

{ #category : #initialization }
FFICompilerPlugin class >> initializeFfiCalloutSelectorsListUpdate [
	collector := PragmaCollector filter: [ :pragma | pragma keyword = #ffiCalloutTranslator ].
	collector reset do: [ :pragma | self addAFfiCalloutSelectorFromPragma: pragma ].
	collector when: PragmaAdded send: #addAFfiCalloutSelectorEvent: to: self.
	collector when: PragmaRemoved send: #removeAFfiCalloutSelectorEvent: to: self
]

{ #category : #private }
FFICompilerPlugin class >> priority [
	^ 2
]

{ #category : #private }
FFICompilerPlugin class >> recompileSendersOf: aCompiledMethod [
	aCompiledMethod senders do: [ :sender | sender recompile ]
]

{ #category : #removing }
FFICompilerPlugin class >> removeAFfiCalloutSelectorEvent: aPragmaRemoved [
	self removeFfiCalloutSelector: aPragmaRemoved pragma methodSelector.
	self recompileSendersOf: aPragmaRemoved pragma method
]

{ #category : #removing }
FFICompilerPlugin class >> removeFfiCalloutSelector: aString [
	FFICalloutSelectors remove: aString ifAbsent: [  ]
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
		- mark ffi methods which use collected selectors that they are FFI"
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
