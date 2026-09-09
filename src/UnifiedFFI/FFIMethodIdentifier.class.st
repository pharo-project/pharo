"
This class tells if a given AST is for an FFI call method or not.
"
Class {
	#name : 'FFIMethodIdentifier',
	#superclass : 'OCStaticASTCompilerPlugin',
	#classVars : [
		'FFICalloutSelectors'
	],
	#category : 'UnifiedFFI-Base',
	#package : 'UnifiedFFI',
	#tag : 'Base'
}

{ #category : 'private' }
FFIMethodIdentifier class >> compilationContextClass [

	^ OCCompilationContext
]

{ #category : 'accessing' }
FFIMethodIdentifier class >> ffiCalloutSelectors [
	<script: 'self ffiCalloutSelectors inspect'>

	^FFICalloutSelectors ifNil: [ self initializeFFICalloutSelectors ]
]

{ #category : 'class initialization' }
FFIMethodIdentifier class >> initialize [

	self
		initializeFFICalloutSelectors;
		recompileSenders
]

{ #category : 'private' }
FFIMethodIdentifier class >> initializeFFICalloutSelectors [

	FFICalloutSelectors := (Pragma allNamed: #ffiCalloutTranslator) collect: [ :pragma | pragma method selector ] as: IdentitySet
]

{ #category : 'private' }
FFIMethodIdentifier class >> priority [
	^ 2
]

{ #category : 'private' }
FFIMethodIdentifier class >> recompileSenders [

	self ffiCalloutSelectors do: [ :selector | selector senders do: [ :sender | sender recompile ] ]
]

{ #category : 'private - accessing' }
FFIMethodIdentifier >> ffiCalloutSelectors [

	^ self class ffiCalloutSelectors
]

{ #category : 'api' }
FFIMethodIdentifier >> transformsFFIMethod [

	(ast hasPragmaNamed: #ffiCalloutTranslator)
		ifTrue: [ ^ false ].
	ast nodesDo: [ :each |
			each isMessage ifTrue: [
				(self ffiCalloutSelectors includes: each selector)
					ifTrue: [ ^ true ] ] ].
	^ false
]
