"
I am the root of a document composite.
"
Class {
	#name : #PRDocument,
	#superclass : #PRDocumentGroup,
	#category : #'Pillar-Core-Model'
}

{ #category : #visiting }
PRDocument >> accept: aVisitor [
	^ aVisitor visitDocument: self
]

{ #category : #'common-properties' }
PRDocument >> authors [
	^ self propertyAt: 'authors' ifAbsent: [ self defaultAuthors ]
]

{ #category : #'common-properties' }
PRDocument >> date [
	^ (self propertyAt: 'date' ifAbsent: [ self defaultDate ]) asDate
]

{ #category : #'common-defaults' }
PRDocument >> defaultAuthors [
	^ ''
]

{ #category : #'common-defaults' }
PRDocument >> defaultDate [
	^ Date today
]

{ #category : #'common-defaults' }
PRDocument >> defaultNext [
	^ nil
]

{ #category : #'common-properties' }
PRDocument >> next [
	^ self propertyAt: 'next' ifAbsent: [ self defaultNext ]
]

{ #category : #'common-properties' }
PRDocument >> next: aDocument [
	^ self propertyAt: 'next' put: aDocument
]
