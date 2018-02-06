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
	^ self ffiCall: #(String strcpy #(String dest , String aString))
]

{ #category : #tests }
FFICompilerPluginTests >> ffiCopyStringWithError: aString to: destError [
	^ self ffiCall: #(String strcpy #(String dest , String aString)) 
]

{ #category : #tests }
FFICompilerPluginTests >> ffiLibraryName [
	^ LibC
]

{ #category : #running }
FFICompilerPluginTests >> setUp [
	self class recompile
]

{ #category : #tests }
FFICompilerPluginTests >> testDecompilationOfFFIMethodShouldHaveNamedArgs [
	| decompiledFFIMethod |
	decompiledFFIMethod := (FFICompilerPluginTests >> #ffiCopyString:to:) decompile.
	self assert: decompiledFFIMethod argumentNames equals: #(#aString #dest)
]

{ #category : #tests }
FFICompilerPluginTests >> testExecutionOfWrongFFIMethodShouldRaiseAnError [
	self
		should: [ 	| buffer result |
	buffer := ByteArray new: 12.
	result := self ffiCopyStringWithError: 'Hello World!' to: buffer. ]
		raise: FFIVariableNameNotFound
]

{ #category : #tests }
FFICompilerPluginTests >> testMethodCall [
	| buffer |
	buffer := ByteArray new: 12.
	self assert: (self ffiCopyString: 'Hello World!' to: buffer) equals: 'Hello World!'
]

{ #category : #tests }
FFICompilerPluginTests >> testThatFFIAdditionalStateIsFilledCorrectlyWith2Parameters [
	| ffiCallMethod |
	ffiCallMethod := self class >> #ffiCopyString:to:.
	self assert: ffiCallMethod properties notNil.
	self assert: (ffiCallMethod propertyAt: #argumentNames) equals: #(#aString #dest)
]
