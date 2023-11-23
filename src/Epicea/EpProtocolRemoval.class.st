"
I represent the removal of a protocol of a class or a trait.
"
Class {
	#name : 'EpProtocolRemoval',
	#superclass : 'EpProtocolChange',
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'visitor' }
EpProtocolRemoval >> accept: aVisitor [
	^ aVisitor visitProtocolRemoval: self
]
