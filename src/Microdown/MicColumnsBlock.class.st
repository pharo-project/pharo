Class {
	#name : #MicColumnsBlock,
	#superclass : #MicEnvironmentBlock,
	#category : #'Microdown-Extensions'
}

{ #category : #accessing }
MicColumnsBlock class >> tag [
	^ #columns
]

{ #category : #visiting }
MicColumnsBlock >> accept: aVisitor [
	^ aVisitor visitColumns: self
]
