Class {
	#name : #FFICompilerPluginTests,
	#superclass : #TestCase,
	#category : #'UnifiedFFI-Tests'
}

{ #category : #compiler }
FFICompilerPluginTests class >> compiler [
	"the example plugin is active for this class"

	^ super compiler addPlugin: FFICompilerPlugin
]

{ #category : #tests }
FFICompilerPluginTests >> ffiCopyString: aString to: dest [
<toto>
	^ self ffiCall: #( String strcpy ( String dest, String aString  ) )   
]

{ #category : #tests }
FFICompilerPluginTests >> test [
	| ffiCallMethod |
	ffiCallMethod := self class >> #ffiCopyString:to:.
	self assert: ffiCallMethod properties class equals: FFIAdditionalFFIMethodState.
	self assert: ffiCallMethod properties argsNames equals: #(#aString #dest).
	self halt
]
