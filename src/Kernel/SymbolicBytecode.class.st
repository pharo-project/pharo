"
I model a single bytecode. I know my value and my offset in the compiledMethod and know the compiledMethod I come from

I am a relatively heavyweight object, I am created on demand for inspecting bytecodes.

See #symbolicBytecode.
"
Class {
	#name : #SymbolicBytecode,
	#superclass : #Object,
	#instVars : [
		'description',
		'method',
		'offset',
		'bytes'
	],
	#category : #'Kernel-Methods'
}

{ #category : #comparing }
SymbolicBytecode >> = anObject [

	self == anObject
		ifTrue: [^ true].

	(self class == anObject class)
		ifFalse: [^ false].

	^ (self bytes = anObject bytes) and: [ 
		(self description = anObject description) and: [ 
			(self method = anObject method) and: [ 
				self offset = anObject offset ] ] ]
]

{ #category : #accessing }
SymbolicBytecode >> bytes [
	^ bytes
]

{ #category : #accessing }
SymbolicBytecode >> bytes: anObject [
	bytes := anObject
]

{ #category : #accessing }
SymbolicBytecode >> description [
	^ description
]

{ #category : #accessing }
SymbolicBytecode >> description: anObject [
	description := anObject
]

{ #category : #comparing }
SymbolicBytecode >> hash [

	^ ((self bytes hash bitXor: self description hash) bitXor: 
			self method hash) bitXor: self offset hash 
]

{ #category : #accessing }
SymbolicBytecode >> method [
	^ method
]

{ #category : #accessing }
SymbolicBytecode >> method: anObject [
	method := anObject
]

{ #category : #accessing }
SymbolicBytecode >> offset [
	^ offset
]

{ #category : #accessing }
SymbolicBytecode >> offset: anObject [
	offset := anObject
]

{ #category : #printing }
SymbolicBytecode >> printOn: aStream [
	aStream print: offset; space.
	aStream nextPut: $<.
	bytes do: [:each | | code |
		code := each radix: 16.
			aStream nextPut: (code size < 2
				ifTrue: [$0]
				ifFalse: [code at: 1]).
			aStream nextPut: code last] separatedBy: [ aStream space].
	aStream nextPut: $>;space.
	aStream nextPutAll: description
]

{ #category : #mapping }
SymbolicBytecode >> sourceInterval [
	^self sourceNode sourceInterval
]

{ #category : #mapping }
SymbolicBytecode >> sourceNode [
	^self method sourceNode sourceNodeForPC: self offset
]
