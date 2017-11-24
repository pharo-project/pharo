"
I represent the addition of a protocol of a class or a trait.
"
Class {
	#name : #EpProtocolAddition,
	#superclass : #EpProtocolChange,
	#category : #'Epicea-Model'
}

{ #category : #visitor }
EpProtocolAddition >> accept: aVisitor [
	^ aVisitor visitProtocolAddition: self
]
