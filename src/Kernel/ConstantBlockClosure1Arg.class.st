"
one-arg block
"
Class {
	#name : #ConstantBlockClosure1Arg,
	#superclass : #ConstantBlockClosure,
	#type : #variable,
	#category : #'Kernel-Methods'
}

{ #category : #accessing }
ConstantBlockClosure1Arg >> value [
	self numArgsError: 0
]

{ #category : #accessing }
ConstantBlockClosure1Arg >> value: anObject [
	^literal
]
