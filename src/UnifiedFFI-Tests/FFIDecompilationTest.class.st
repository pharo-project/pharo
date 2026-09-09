"
Tests for FFICompilerPlugin
"
Class {
	#name : 'FFIDecompilationTest',
	#superclass : 'TestCase',
	#category : 'UnifiedFFI-Tests-Tests',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
FFIDecompilationTest >> ffiCopyString: aString to: dest [

	^ self ffiCall: #(String strcpy(void* dest , String aString))
]

{ #category : 'tests' }
FFIDecompilationTest >> ffiCopyStringWithError: aString to: destError [
	"This method explicitly uses a non-matching variable dest in the FFI call to provoke an error"

	^ self ffiCall: #(String strcpy #(String dest , String aString))
]

{ #category : 'tests' }
FFIDecompilationTest >> ffiLibraryName [
	^ LibC
]

{ #category : 'running' }
FFIDecompilationTest >> setUp [
	super setUp.
	self class recompile
]

{ #category : 'tests' }
FFIDecompilationTest >> testDecompilationOfFFIMethodShouldHaveNamedArgs [
	| decompiledFFIMethod |
	decompiledFFIMethod := (FFIDecompilationTest >> #ffiCopyString:to:) decompile.
	self assert: decompiledFFIMethod argumentNames equals: #(#aString #dest)
]

{ #category : 'tests' }
FFIDecompilationTest >> testExecutionOfWrongFFIMethodShouldRaiseAnError [
	self
		should: [ | buffer |
			buffer := ByteArray new: 12.
			self ffiCopyStringWithError: 'Hello World!' to: buffer ]
		raise: FFIVariableNameNotFound
]

{ #category : 'tests' }
FFIDecompilationTest >> testFFIMethodDelegatesWhenLookingForHasSelector [

	| buffer ffiMethod |
	"Execute the method to ensure it is compiled as FFI method"
	buffer := ByteArray new: 12.
	self ffiCopyString: 'Hello World!' to: buffer.

	ffiMethod := self class >> #ffiCopyString:to:.

	self assert: (ffiMethod hasSelector: #ffiCall: specialSelectorIndex: nil)
]

{ #category : 'tests' }
FFIDecompilationTest >> testMethodCall [
	| buffer |
	buffer := ByteArray new: 12.
	self assert: (self ffiCopyString: 'Hello World!' to: buffer) equals: 'Hello World!'
]

{ #category : 'tests' }
FFIDecompilationTest >> testThatFFIAdditionalStateIsFilledCorrectlyWith2Parameters [
	| ffiCallMethod |
	ffiCallMethod := self class >> #ffiCopyString:to:.
	self assert: ffiCallMethod properties isNotNil.
	self assert: (ffiCallMethod propertyAt: #argumentNames) equals: #(#aString #dest)
]
