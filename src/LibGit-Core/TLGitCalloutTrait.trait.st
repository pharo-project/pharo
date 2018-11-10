Trait {
	#name : #TLGitCalloutTrait,
	#category : #'LibGit-Core-Traits'
}

{ #category : #'ffi-calls' }
TLGitCalloutTrait classSide >> call: fnSpec [
	^ (self ffiCalloutIn: thisContext sender)
		cdecl;
		function: fnSpec module: self ffiLibrary
]

{ #category : #'ffi-calls' }
TLGitCalloutTrait classSide >> call: fnSpec options: options [
	^ (self ffiCalloutIn: thisContext sender)
		cdecl;
		options: options;
		function: fnSpec module: self ffiLibrary
]

{ #category : #resources }
TLGitCalloutTrait classSide >> ffiLibrary [

	^LGitLibrary 
]

{ #category : #resources }
TLGitCalloutTrait classSide >> ffiLibraryName [

	^self ffiLibrary
]

{ #category : #'argument validation' }
TLGitCalloutTrait classSide >> validate: anObject shouldBe: aClass [
	"I allow for nil values."
	(anObject isNil or: [ anObject isKindOf: aClass ])
		ifFalse: [ LGitIllegalArgument signalWith: anObject shouldBe: aClass ]
]

{ #category : #'exit code handling' }
TLGitCalloutTrait classSide >> withReturnHandlerDo: callBlock [
	^ callBlock value handleLGitReturnCode
]

{ #category : #'ffi-calls' }
TLGitCalloutTrait >> call: fnSpec [
	<ffiCalloutTranslator>
	^ (self safeFFICalloutIn: thisContext sender)
		cdecl;
		function: fnSpec module: self ffiLibraryName
]

{ #category : #'ffi-calls' }
TLGitCalloutTrait >> call: fnSpec options: options [
	<ffiCalloutTranslator>
	^ (self safeFFICalloutIn: thisContext sender)
		cdecl;
		options: options;
		function: fnSpec module: self ffiLibraryName
]

{ #category : #'ffi-calls' }
TLGitCalloutTrait >> callUnchecked: fnSpec options: options [
	<ffiCalloutTranslator>
	^ (self ffiCalloutIn: thisContext sender)
		cdecl;
		options: options;
		function: fnSpec module: self ffiLibraryName
]

{ #category : #resources }
TLGitCalloutTrait >> ffiLibrary [

	^self class ffiLibrary
]

{ #category : #resources }
TLGitCalloutTrait >> ffiLibraryName [

	^self class ffiLibrary
]

{ #category : #resources }
TLGitCalloutTrait >> ffiNull [

	^FFIExternalObject null
]

{ #category : #testing }
TLGitCalloutTrait >> ifNotReady: aBlock [
	"use extra block so compiler can optimize"
	self isReady ifFalse: [ aBlock value ]
]

{ #category : #testing }
TLGitCalloutTrait >> isReady [
	^ true
]

{ #category : #'ffi-calls' }
TLGitCalloutTrait >> safeFFICalloutIn: aContext [
	^ LGitSafeFFICalloutAPI inContext: aContext
]

{ #category : #signalling }
TLGitCalloutTrait >> signalIfNotReady [
	self ifNotReady: [ self signalNotReady ]
]

{ #category : #signalling }
TLGitCalloutTrait >> signalNotReady [
	LGitObjectNotInitialized signalWith: self
]

{ #category : #'argument validation' }
TLGitCalloutTrait >> validate: anObject shouldBe: aClass [
	self class validate: anObject shouldBe: aClass

]

{ #category : #'exit code handling' }
TLGitCalloutTrait >> withReturnHandlerDo: callBlock [
	^ callBlock value handleLGitReturnCode
]
