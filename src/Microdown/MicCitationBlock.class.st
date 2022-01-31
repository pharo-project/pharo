Class {
	#name : #MicCitationBlock,
	#superclass : #MicAnnotationBlock,
	#category : #'Microdown-Extensions'
}

{ #category : #accessing }
MicCitationBlock class >> tag [

	^ #citation
]

{ #category : #visiting }
MicCitationBlock >> accept: aVisitor [
	aVisitor visitCitation: self
]

{ #category : #accessing }
MicCitationBlock >> ref [
	^ arguments at: 'ref' 
		ifAbsent: [ ^ '' ]
]
