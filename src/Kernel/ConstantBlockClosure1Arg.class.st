"
one-arg block
"
Class {
	#name : #ConstantBlockClosure1Arg,
	#superclass : #ConstantBlockClosure,
	#type : #variable,
	#category : #'Kernel-Methods'
}

{ #category : #evaluating }
ConstantBlockClosure1Arg >> value [
	self numArgsError: 0
]

{ #category : #evaluating }
ConstantBlockClosure1Arg >> value: anObject [
	^literal
]

{ #category : #evaluating }
ConstantBlockClosure1Arg >> valueWithArguments: anArray [
	(anArray size ~= 1) ifTrue: [self numArgsError: anArray size].
	^literal
]
