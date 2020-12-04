"
I am an anchor within a document. I am used as a reference point within a large document.
"
Class {
	#name : #PRAnchor,
	#superclass : #PRDocumentItem,
	#instVars : [
		'name'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #testing }
PRAnchor class >> isAbstract [
	^ false
]

{ #category : #'instance creation' }
PRAnchor class >> named: aString [
	^ self new name: aString
]

{ #category : #comparing }
PRAnchor >> = anObject [
	^ super = anObject
		and: [ self name = anObject name ]
]

{ #category : #visiting }
PRAnchor >> accept: aVisitor [
	^ aVisitor visitAnchor: self
]

{ #category : #comparing }
PRAnchor >> hash [
	^ super hash bitXor: self name hash
]

{ #category : #accessing }
PRAnchor >> name [
	^ name
]

{ #category : #accessing }
PRAnchor >> name: aString [
	name := aString trimBoth	"We do trimBoth to avoid to have unexpected space before or after an anchor."
]
