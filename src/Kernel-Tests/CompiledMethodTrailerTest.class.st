"
SUnit tests for CompiledMethodTrailer
"
Class {
	#name : #CompiledMethodTrailerTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Methods'
}

{ #category : #utilities }
CompiledMethodTrailerTest >> testEmbeddingSourceCode: sourceCode [

	| trailer newTrailer |
	
	trailer := CompiledMethodTrailer new.
	trailer embeddedSourceCode: sourceCode.
	newTrailer := trailer testEncoding.
	
	self assert: (#(EmbeddedSource EmbeddedSourceWide) includes: trailer kind).
	self assert: newTrailer sourceCode equals: sourceCode.

	"the last bytecode index must be at 0"
	self assert: newTrailer endPC equals: 0.
]

{ #category : #tests }
CompiledMethodTrailerTest >> testEmbeddingSourceCodeWide [	
	"Test that wide strings survive"
	
	self testEmbeddingSourceCode: 'doit ^''Под южно дърво, цъфтящо в синьо, бягаше малко пухкаво зайче.'' '.
	
]

{ #category : #tests }
CompiledMethodTrailerTest >> testEncodingNoTrailer [
	| trailer |
	trailer := CompiledMethodTrailer new.

	"by default it should be a no-trailer, 4 byte wide so it can be used to store a sourcePointer"
	self assert: trailer kind identicalTo: #NoTrailer.
	self assert: trailer size equals: 4.

	trailer := trailer testEncoding.

	self assert: trailer kind identicalTo: #NoTrailer.
	self assert: trailer size equals: 4.
	"the last bytecode index must be at 0"
	self assert: trailer endPC equals: 0
]

{ #category : #tests }
CompiledMethodTrailerTest >> testEncodingSourcePointer [
	| trailer |
	trailer := CompiledMethodTrailer new.

	CompiledMethod
		allInstancesDo: [ :method | 
			| ptr |
			trailer method: method.
			self assert: (ptr := method sourcePointer) identicalTo: trailer sourcePointer.
			"the last bytecode index must be at 0"
			ptr ~= 0 ifTrue: [ self assert: method endPC equals: trailer endPC ] ]
]

{ #category : #tests }
CompiledMethodTrailerTest >> testEncodingVarLengthSourcePointer [

	| trailer newTrailer |
	
	trailer := CompiledMethodTrailer new.
	
	trailer sourcePointer: 1.
	newTrailer := trailer testEncoding.
	
	self assert: newTrailer sourcePointer equals: 1.
	
	trailer sourcePointer: 16r100000000000000.
	newTrailer := trailer testEncoding.
	self assert: newTrailer sourcePointer equals: 16r100000000000000.
	"the last bytecode index must be at 0"
	self assert: newTrailer endPC equals: 0
]

{ #category : #tests }
CompiledMethodTrailerTest >> testEncodingZeroSourcePointer [

	| trailer |
	
	trailer := CompiledMethodTrailer new.

	self assert: (trailer sourcePointer: 0) testEncoding sourcePointer equals: 0
	
]
