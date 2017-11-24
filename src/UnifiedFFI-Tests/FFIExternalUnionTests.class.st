Class {
	#name : #FFIExternalUnionTests,
	#superclass : #TestCase,
	#category : #'UnifiedFFI-Tests'
}

{ #category : #tests }
FFIExternalUnionTests >> testUnionAccess [

	| union |
	
	union := FFITestUnion new.
	self deny: union getHandle class = ExternalAddress.	
	self deny: union isNull.
	
	union long: 16r04030201.
	self assert: (union byte = 16r01).
	self assert: (union short = 16r0201).
	self assert: (union long = 16r04030201).
	self assert: (union float ~= 0).
	self assert: (union double ~= 0).
	
		self flag: #todo. "This is not yet implemented"
	"struct int64: 123456789101112."

]
