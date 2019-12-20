"
Tests for external unions
"
Class {
	#name : #FFIExternalUnionTest,
	#superclass : #TestCase,
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #tests }
FFIExternalUnionTest >> testUnionAccess [
	| union |
	union := FFITestUnion new.
	self deny: union getHandle class equals: ExternalAddress.
	self deny: union isNull.

	union long: 16r04030201.
	self assert: union byte equals: 16r01.
	self assert: union short equals: 16r0201.
	self assert: union long equals: 16r04030201.
	self assert: union float ~= 0.
	self assert: union double ~= 0.

	self flag: #pharoTodo	"This is not yet implemented"
	"struct int64: 123456789101112."
]
