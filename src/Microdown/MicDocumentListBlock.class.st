Class {
	#name : #MicDocumentListBlock,
	#superclass : #MicAnnotationBlock,
	#category : #'Microdown-Extensions'
}

{ #category : #accessing }
MicDocumentListBlock class >> tag [ 
	^ #documentlist
]

{ #category : #visiting }
MicDocumentListBlock >> accept: aVisitor [

	^ aVisitor visitDocListAnnotation: self
]

{ #category : #visiting }
MicDocumentListBlock >> limit [

	^ self arguments at: #limit ifAbsent: [ '3' ]
]

{ #category : #visiting }
MicDocumentListBlock >> path [

	^ self arguments at: #path
]

{ #category : #visiting }
MicDocumentListBlock >> sort [

	^ self arguments at: #sort ifAbsent: [ '' ]
]

{ #category : #visiting }
MicDocumentListBlock >> summaryMaxSize [

	^ self arguments at: #summaryMaxSize
		ifPresent: [ :size | size asInteger ]
		ifAbsent: [ 500 ]
]

{ #category : #visiting }
MicDocumentListBlock >> templates [

	^ self arguments at: #templates ifAbsent: [ #() ]
]
