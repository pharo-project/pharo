Class {
	#name : #MicColumnBlock,
	#superclass : #MicEnvironmentBlock,
	#category : #'Microdown-Extensions'
}

{ #category : #accessing }
MicColumnBlock class >> tag [
	^ #column
]

{ #category : #accessing }
MicColumnBlock >> accept: aVisitor [
	^ aVisitor visitColumn: self
]

{ #category : #accessing }
MicColumnBlock >> width [
	^ arguments at: #width
]

{ #category : #accessing }
MicColumnBlock >> width: anObject [
	arguments at: #width put: anObject
]
