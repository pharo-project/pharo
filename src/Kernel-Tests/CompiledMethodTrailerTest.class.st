"
SUnit tests for CompiledMethodTrailer
"
Class {
	#name : #CompiledMethodTrailerTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Methods'
}

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
CompiledMethodTrailerTest >> testEncodingZeroSourcePointer [

	| trailer |

	trailer := CompiledMethodTrailer new.

	self assert: (trailer sourcePointer: 0) testEncoding sourcePointer equals: 0
]
