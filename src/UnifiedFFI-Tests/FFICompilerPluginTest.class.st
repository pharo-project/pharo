"
Tests for FFICompilerPlugin
"
Class {
	#name : 'FFICompilerPluginTest',
	#superclass : 'TestCase',
	#category : 'UnifiedFFI-Tests-Tests',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Tests'
}

{ #category : 'compiler' }
FFICompilerPluginTest class >> compiler [
	"the example plugin is active for this class"

	^ super compiler addPlugin: FFICompilerPlugin
]

{ #category : 'tests' }
FFICompilerPluginTest >> ffiCopyString: aString to: dest [
	^ self ffiCall: #(String strcpy #(void* dest , String aString))
]

{ #category : 'tests' }
FFICompilerPluginTest >> ffiCopyStringWithError: aString to: destError [
	"This method explicitly uses a non-matching variable dest in the FFI call to provoke an error"

	^ self ffiCall: #(String strcpy #(String dest , String aString))
]

{ #category : 'tests' }
FFICompilerPluginTest >> ffiLibraryName [
	^ LibC
]

{ #category : 'running' }
FFICompilerPluginTest >> setUp [
	super setUp.
	self class recompile
]

{ #category : 'tests' }
FFICompilerPluginTest >> testDecompilationOfFFIMethodShouldHaveNamedArgs [
	| decompiledFFIMethod |
	decompiledFFIMethod := (FFICompilerPluginTest >> #ffiCopyString:to:) decompile.
	self assert: decompiledFFIMethod argumentNames equals: #(#aString #dest)
]

{ #category : 'tests' }
FFICompilerPluginTest >> testExecutionOfWrongFFIMethodShouldRaiseAnError [
	self
		should: [ | buffer |
			buffer := ByteArray new: 12.
			self ffiCopyStringWithError: 'Hello World!' to: buffer ]
		raise: FFIVariableNameNotFound
]

{ #category : 'tests' }
FFICompilerPluginTest >> testFFIMethodDelegatesWhenLookingForHasSelector [

	| buffer ffiMethod |
	"Execute the method to ensure it is compiled as FFI method"
	buffer := ByteArray new: 12.
	self ffiCopyString: 'Hello World!' to: buffer.

	ffiMethod := FFICompilerPluginTest >> #ffiCopyString:to:.

	self assert: (ffiMethod hasSelector: #ffiCall: specialSelectorIndex: nil)
]

{ #category : 'tests' }
FFICompilerPluginTest >> testMethodCall [
	| buffer |
	buffer := ByteArray new: 12.
	self assert: (self ffiCopyString: 'Hello World!' to: buffer) equals: 'Hello World!'
]

{ #category : 'tests' }
FFICompilerPluginTest >> testThatFFIAdditionalStateIsFilledCorrectlyWith2Parameters [
	| ffiCallMethod |
	ffiCallMethod := self class >> #ffiCopyString:to:.
	self assert: ffiCallMethod properties isNotNil.
	self assert: (ffiCallMethod propertyAt: #argumentNames) equals: #(#aString #dest)
]
