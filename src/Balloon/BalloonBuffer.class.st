"
BalloonBuffer is a repository for primitive data used by the BalloonEngine.
"
Class {
	#name : #BalloonBuffer,
	#superclass : #Object,
	#type : #words,
	#category : 'Balloon-Engine'
}

{ #category : #'instance creation' }
BalloonBuffer class >> new [
	^self new: 256.
]

{ #category : #accessing }
BalloonBuffer >> at: index [
	"For simulation only"
	| word |
	word := self basicAt: index.
	word < 16r3FFFFFFF ifTrue:[^word]. "Avoid LargeInteger computations"
	^word >= 16r80000000	"Negative?!"
		ifTrue:["word - 16r100000000"
				(word bitInvert32 + 1) negated]
		ifFalse:[word]
]

{ #category : #accessing }
BalloonBuffer >> at: index put: anInteger [
	"For simulation only"
	| word |
	anInteger < 0
		ifTrue:["word := 16r100000000 + anInteger"
				word := (anInteger + 1) negated bitInvert32]
		ifFalse:[word := anInteger].
	self  basicAt: index put: word.
	^anInteger
]

{ #category : #accessing }
BalloonBuffer >> floatAt: index [
	"For simulation only"
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)
]

{ #category : #accessing }
BalloonBuffer >> floatAt: index put: value [
	"For simulation only"
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value
]
