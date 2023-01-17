"
3-arg block
"
Class {
	#name : #ConstantBlockClosure3Arg,
	#superclass : #ConstantBlockClosure,
	#type : #variable,
	#category : #'Kernel-Methods'
}

{ #category : #evaluating }
ConstantBlockClosure3Arg >> value [
	self numArgsError: 0
]

{ #category : #evaluating }
ConstantBlockClosure3Arg >> value: firstArg value: secondArg value: thirdArg [
	^literal
]

{ #category : #evaluating }
ConstantBlockClosure3Arg >> valueWithArguments: anArray [
	(anArray size ~= 3) ifTrue: [self numArgsError: anArray size].
	^literal
]
