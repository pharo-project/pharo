"
I am a FFICompilerPlugin. I am a plugin for the OpalCompiler for the compiler that makes the compiled method store the arguments names 
to be used for FFI when the sources are not loaded or unloaded.
I can be activated with the command FFICompilerPlugin install.
I am pragma-based to detect the methods where the arguments names should be remembered.
The pragma should be added in the FFI API methods, i.e., the methods that are called by the FFI methods where the arguments have to be remembered.
Example:

This FFI method should remember the name of the argument named config:

repository_config: config
	^ self
		call: #(#LGitReturnCodeEnum #git_repository_config #(#LGitConfig #* #config #, #self))
		options: #()

So, the FFI function should wear the pragma <ffiCalloutTranslator>:

call: fnSpec options: options
	<ffiCalloutTranslator>
	^ (self safeFFICalloutIn: thisContext sender)
		cdecl;
		options: options;
		function: fnSpec module: self ffiLibraryName

To remove to be able to remove the sources (.changes and .sources), you only have to activate the plugin, 
no recompilation is necessary. You can even import new FFI methods or change the FFI API.

N.B: Users that redefine the FFI API (like TLGitCalloutTrait >> call:options:) also have to wear the pragma.
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

{ #category : #installation }
FFICompilerPlugin class >> install [
	<script>
	(CompilationContext defaultTransformationPlugins includes: self)
		ifFalse: [ CompilationContext addDefaultTransformationPlugin: self.
			self recompileSenders ]
]

{ #category : #private }
FFICompilerPlugin class >> priority [
	^ 2
]

{ #category : #installation }
FFICompilerPlugin class >> recompileSenders [
	collector reset do: [ :pragma | self recompileSendersOf: pragma method ]
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

{ #category : #installation }
FFICompilerPlugin class >> uninstall [
	<script>
	CompilationContext removeDefaultTransformationPlugin: self.
	self recompileSenders
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
	self transformsFFIMethod ifFalse: [ ^ast].
	 
	ast ensureCachedArgumentNames.
	ast methodPropertyAt: #isFFIMethod put: true.
	^ast
]

{ #category : #api }
FFICompilerPlugin >> transformsFFIMethod [

 	ast nodesDo: [:each | 
      each isMessage ifTrue: [    
    		(self ffiCalloutSelectors includes: each selector) ifTrue: [^true]]].
  ^false
]
