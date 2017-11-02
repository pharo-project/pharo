"
I represent the removal of a protocol of a class or a trait.
"
Class {
	#name : #EpProtocolRemoval,
	#superclass : #EpProtocolChange,
	#category : 'Epicea-Model'
}

{ #category : #visitor }
EpProtocolRemoval >> accept: aVisitor [
	^ aVisitor visitProtocolRemoval: self
]
