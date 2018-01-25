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
FFICompilerPluginTests >> ffiCopyString2: aString to: dest [
	^ self ffiCall: #( String strcpy ( String dest, String aString  ) )       
]

{ #category : #tests }
FFICompilerPluginTests >> ffiCopyString: aString to: dest [
	^ self ffiCall: #(String strcpy ( String dest, String aString  ) )
]

{ #category : #tests }
FFICompilerPluginTests >> ffiLibraryName [
	^ LibC
]

{ #category : #tests }
FFICompilerPluginTests >> testMethodCall [
	| buffer result |
	buffer := ByteArray new: 12.
	result := self ffiCopyString2: 'Hello World!' to: buffer.
]

{ #category : #tests }
FFICompilerPluginTests >> testRecompilationOfWrongFFIMethodShouldRaiseAnError [
	self
		should: [ self
				class compile:
					'ffiCopyString: aString to: dest2
	^ self ffiCall: #(String strcpy ( String dest, String aString  ) )' ]
		raise: NotFound
]

{ #category : #tests }
FFICompilerPluginTests >> testThatFFIAdditionalStateIsFilledCorrectlyWith2Parameters [
	| ffiCallMethod |
	ffiCallMethod := self class >> #ffiCopyString:to:.
	self assert: ffiCallMethod properties class equals: FFIAdditionalFFIMethodState.
	self assert: ffiCallMethod properties argsNames equals: #(#aString #dest)
]
