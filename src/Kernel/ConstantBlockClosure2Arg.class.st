"
2-arg block

"
Class {
	#name : #ConstantBlockClosure2Arg,
	#superclass : #ConstantBlockClosure,
	#type : #variable,
	#category : #'Kernel-Methods'
}

{ #category : #evaluating }
ConstantBlockClosure2Arg >> value [
	self numArgsError: 0
]

{ #category : #evaluating }
ConstantBlockClosure2Arg >> value: firstArg value: secondArg [
	^literal
]

{ #category : #evaluating }
ConstantBlockClosure2Arg >> valueWithArguments: anArray [
	(anArray size ~= 2) ifTrue: [self numArgsError: anArray size].
	^literal
]
